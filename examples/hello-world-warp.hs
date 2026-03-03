module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Hypermedia.Datastar
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)

newtype Signals = Signals {delay :: Int}

instance FromJSON Signals where
  parseJSON = withObject "Signals" $ \o ->
    Signals <$> o .: "delay"

message :: String
message = "Hello, world!"

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) -> read p
        _ -> 3000
  htmlContent <- BS.readFile "examples/hello-world.html"
  putStrLn $ "Listening on http://localhost:" <> show port
  Warp.run port (app htmlContent)

app :: BS.ByteString -> Application
app htmlContent req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] (LBS.fromStrict htmlContent)
    ("GET", ["hello-world"]) ->
      handleHelloWorld req respond
    _ ->
      respond $ responseLBS status404 [] "Not found"

handleHelloWorld :: Wai.Request -> (Wai.Response -> IO b) -> IO b
handleHelloWorld req respond = do
  signalsResult <- readSignals req :: IO (Either String Signals)
  case signalsResult of
    Left _ -> respond $ responseLBS status404 [] "Bad signals"
    Right signals -> respond $ sseResponse $ \gen ->
      mapM_
        ( \i -> do
            let html = "<div id='message'>" <> T.pack (take i message) <> "</div>"
            sendPatchElements gen (patchElements html)
            threadDelay (delay signals * 1000)
        )
        [1 .. length message]
