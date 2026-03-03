module Main (main) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, registerDelay, retry, writeTVar)
import Control.Monad (forever)
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

newtype Signals = Signals {_delay :: Int}
  deriving (Show)

instance FromJSON Signals where
  parseJSON = withObject "Signals" $ \o ->
    Signals <$> o .: "delay"

data SharedState = SharedState
  { _delayVar :: TVar Int
  , _versionVar :: TVar Int
  }

message :: String
message = "Hello, world!"

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) -> read p
        _ -> 3000
  htmlContent <- BS.readFile "examples/hello-world-channel.html"
  state <- SharedState <$> newTVarIO 400 <*> newTVarIO 0
  putStrLn $ "Listening on http://localhost:" <> show port
  Warp.run port (app htmlContent state)

app :: BS.ByteString -> SharedState -> Application
app htmlContent state req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] (LBS.fromStrict htmlContent)
    ("GET", ["set-delay"]) ->
      handleSetDelay state req respond
    ("GET", ["hello-world"]) ->
      handleHelloWorld state respond
    _ ->
      respond $ responseLBS status404 [] "Not found"

handleSetDelay :: SharedState -> Wai.Request -> (Wai.Response -> IO b) -> IO b
handleSetDelay state req respond = do
  signalsResult <- readSignals req :: IO (Either String Signals)
  case signalsResult of
    Left _ -> respond $ responseLBS status404 [] "Bad signals"
    Right signals -> do
      atomically $ do
        writeTVar (_delayVar state) (_delay signals)
        v <- readTVar (_versionVar state)
        writeTVar (_versionVar state) (v + 1)
      respond $ sseResponse $ \_ -> pure ()

handleHelloWorld :: SharedState -> (Wai.Response -> IO b) -> IO b
handleHelloWorld state respond =
  respond $ sseResponse $ \gen ->
    forever $ do
      version <- readTVarIO (_versionVar state)
      d <- readTVarIO (_delayVar state)
      animate gen state d version [0 .. length message]

-- Animate character by character; breaks out early if the version changes
-- (i.e. Start was clicked), letting `forever` restart from the beginning.
animate :: ServerSentEventGenerator -> SharedState -> Int -> Int -> [Int] -> IO ()
animate _ _ _ _ [] = pure ()
animate gen state d version (i : is) = do
  let html = "<div id='message'>" <> T.pack (take i message) <> "</div>"
  sendPatchElements gen (patchElements html)
  -- Race the delay against a version change
  timedOut <- registerDelay (d * 1000)
  interrupted <- atomically $ do
    timeout <- readTVar timedOut
    v <- readTVar (_versionVar state)
    case (timeout, v /= version) of
      (_, True) -> pure True -- version changed, interrupt
      (True, _) -> pure False -- delay elapsed, continue
      _ -> retry -- neither yet, keep waiting
  if interrupted
    then pure () -- break out; forever will restart
    else animate gen state d version is
