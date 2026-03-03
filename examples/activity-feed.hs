module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hypermedia.Datastar
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)

data Signals = Signals
  { _sInterval :: Int
  , _sEvents :: Int
  , _sGenerating :: Bool
  , _sTotal :: Int
  , _sDone :: Int
  , _sWarn :: Int
  , _sFail :: Int
  , _sInfo :: Int
  }

instance FromJSON Signals where
  parseJSON = withObject "Signals" $ \o ->
    Signals
      <$> o .: "interval"
      <*> o .: "events"
      <*> o .: "generating"
      <*> o .: "total"
      <*> o .: "done"
      <*> o .: "warn"
      <*> o .: "fail"
      <*> o .: "info"

data Status = Done | Warn | Fail | Info

statusFromText :: Text -> Maybe Status
statusFromText "done" = Just Done
statusFromText "warn" = Just Warn
statusFromText "fail" = Just Fail
statusFromText "info" = Just Info
statusFromText _ = Nothing

statusColor :: Status -> Text
statusColor Done = "green"
statusColor Warn = "yellow"
statusColor Fail = "red"
statusColor Info = "blue"

statusIndicator :: Status -> Text
statusIndicator Done = "Done"
statusIndicator Warn = "Warn"
statusIndicator Fail = "Fail"
statusIndicator Info = "Info"

eventEntry :: Status -> Int -> Text -> IO Text
eventEntry status index source = do
  now <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3Q" now
      color = statusColor status
      indicator = statusIndicator status
  pure $
    "<div id='event-"
      <> T.pack (show index)
      <> "' class='text-"
      <> color
      <> "-500'>"
      <> timestamp
      <> " [ "
      <> indicator
      <> " ] "
      <> source
      <> " event "
      <> T.pack (show index)
      <> "</div>"

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) -> read p
        _ -> 3000
  htmlContent <- BS.readFile "examples/activity-feed.html"
  putStrLn $ "Listening on http://localhost:" <> show port
  Warp.run port (app htmlContent)

app :: BS.ByteString -> Application
app htmlContent req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] (LBS.fromStrict htmlContent)
    ("POST", ["event", "generate"]) ->
      handleGenerate req respond
    ("POST", ["event", statusText])
      | Just status <- statusFromText statusText ->
          handleEvent status req respond
    _ ->
      respond $ responseLBS status404 [] "Not found"

handleGenerate :: Wai.Request -> (Wai.Response -> IO b) -> IO b
handleGenerate req respond = do
  signalsResult <- readSignals req :: IO (Either String Signals)
  case signalsResult of
    Left err -> respond $ responseLBS status404 [] (LBS.fromStrict $ BS8.pack $ "Bad signals: " <> err)
    Right signals -> respond $ sseResponse $ \gen -> do
      sendPatchSignals gen (patchSignals "{\"generating\": true}")

      let loop 0 _ _ = pure ()
          loop n total' done' = do
            let newTotal = total' + 1
                newDone = done' + 1
            html <- eventEntry Done newTotal "Auto"
            sendPatchElements gen $
              (patchElements html){peSelector = Just "#feed", peMode = After}
            sendPatchSignals gen $
              patchSignals $
                "{\"total\": " <> T.pack (show newTotal) <> ", \"done\": " <> T.pack (show newDone) <> "}"
            threadDelay (_sInterval signals * 1000)
            loop (n - 1) newTotal newDone

      loop (_sEvents signals) (_sTotal signals) (_sDone signals)

      sendPatchSignals gen (patchSignals "{\"generating\": false}")

handleEvent :: Status -> Wai.Request -> (Wai.Response -> IO b) -> IO b
handleEvent status req respond = do
  signalsResult <- readSignals req :: IO (Either String Signals)
  case signalsResult of
    Left err -> respond $ responseLBS status404 [] (LBS.fromStrict $ BS8.pack $ "Bad signals: " <> err)
    Right signals -> respond $ sseResponse $ \gen -> do
      let newTotal = _sTotal signals + 1
          counterSignals = case status of
            Done -> "{\"total\": " <> T.pack (show newTotal) <> ", \"done\": " <> T.pack (show (_sDone signals + 1)) <> "}"
            Warn -> "{\"total\": " <> T.pack (show newTotal) <> ", \"warn\": " <> T.pack (show (_sWarn signals + 1)) <> "}"
            Fail -> "{\"total\": " <> T.pack (show newTotal) <> ", \"fail\": " <> T.pack (show (_sFail signals + 1)) <> "}"
            Info -> "{\"total\": " <> T.pack (show newTotal) <> ", \"info\": " <> T.pack (show (_sInfo signals + 1)) <> "}"
      sendPatchSignals gen (patchSignals counterSignals)

      html <- eventEntry status newTotal "Manual"
      sendPatchElements gen $
        (patchElements html){peSelector = Just "#feed", peMode = After}
