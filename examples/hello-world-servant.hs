module Main (main) where

import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Hypermedia.Datastar
import Network.HTTP.Media ((//))
import Network.HTTP.Types (status404)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import System.Environment (getArgs)

data HTML

instance Accept HTML where
  contentType _ = "text" // "html"

instance MimeRender HTML LBS.ByteString where
  mimeRender _ = id

newtype Signals = Signals {delay :: Int}

instance FromJSON Signals where
  parseJSON = withObject "Signals" $ \o ->
    Signals <$> o .: "delay"

message :: String
message = "Hello, world!"

type API =
  Get '[HTML] LBS.ByteString
    :<|> "hello-world" :> Raw

server :: BS.ByteString -> Server API
server htmlContent =
  serveIndex htmlContent
    :<|> serveHelloWorld

serveIndex :: BS.ByteString -> Handler LBS.ByteString
serveIndex htmlContent =
  pure $ LBS.fromStrict htmlContent

serveHelloWorld :: Tagged Handler Application
serveHelloWorld = Tagged $ \req respond -> do
  signalsResult <- readSignals req :: IO (Either String Signals)
  case signalsResult of
    Left _ -> respond $ Wai.responseLBS status404 [] "Bad signals"
    Right signals -> respond $ sseResponse $ \gen ->
      mapM_
        ( \i -> do
            let html = "<div id='message'>" <> T.pack (take i message) <> "</div>"
            sendPatchElements gen (patchElements html)
            threadDelay (delay signals * 1000)
        )
        [1 .. length message]

app :: BS.ByteString -> Application
app htmlContent = serve (Proxy :: Proxy API) (server htmlContent)

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) -> read p
        _ -> 3000
  htmlContent <- BS.readFile "examples/hello-world.html"
  putStrLn $ "Listening on http://localhost:" <> show port
  Warp.run port (app htmlContent)
