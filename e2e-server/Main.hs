module Main where

import Data.Aeson (FromJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hypermedia.Datastar
import Lucid
import Lucid.Base (makeAttributes)
import Network.HTTP.Types (status200, status404)
import Network.Wai qualified as WAI
import Network.Wai.Handler.Warp qualified as Warp

newtype Greeting = Greeting {greeting :: T.Text}
  deriving (Generic)
  deriving anyclass (FromJSON)

app :: WAI.Request -> (WAI.Response -> IO b) -> IO b
app req respond = case (WAI.requestMethod req, WAI.pathInfo req) of
  ("GET", []) ->
    respond $ WAI.responseLBS status200 [("Content-Type", "text/html")] testPage
  ("GET", ["sse", "patch-elements"]) ->
    respond $ sseResponse nullLogger $ \gen ->
      sendPatchElements gen (patchElements "<div id=\"pe-result\">Patched Content</div>")
  ("GET", ["sse", "patch-signals"]) ->
    respond $ sseResponse nullLogger $ \gen ->
      sendPatchSignals gen (patchSignals "{\"message\":\"Signal Updated\"}")
  ("GET", ["sse", "execute-script"]) ->
    respond $ sseResponse nullLogger $ \gen ->
      sendExecuteScript gen (executeScript "document.getElementById('es-result').textContent = 'Script Executed'")
  ("GET", ["sse", "read-signals"]) ->
    respond $ sseResponse nullLogger $ \gen -> do
      result <- readSignals req
      case result of
        Right (signals :: Greeting) ->
          sendPatchElements gen (patchElements ("<div id=\"rs-result\">" <> greeting signals <> "</div>"))
        Left err ->
          sendPatchElements gen (patchElements ("<div id=\"rs-result\">Error: " <> T.pack err <> "</div>"))
  ("GET", ["sse", "multiple-events"]) ->
    respond $ sseResponse nullLogger $ \gen -> do
      sendPatchElements gen (patchElements "<div id=\"me-result\">Event 1</div>")
      sendPatchElements gen (patchElements "<div id=\"me-result\">Event 2</div>")
      sendPatchElements gen (patchElements "<div id=\"me-result\">Event 3</div>")
  _ ->
    respond $ WAI.responseLBS status404 [] "Not found"

testPage :: LBS.ByteString
testPage = renderBS $ doctypehtml_ $ do
  head_ $ do
    title_ "datastar-hs e2e tests"
    script_ [type_ "module", src_ "https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"] ("" :: T.Text)
  body_ [makeAttributes "data-signals" "{}"] $ do
    h1_ "datastar-hs E2E Test Page"

    section_ [id_ "patch-elements-test"] $ do
      h2_ "Patch Elements"
      button_ [id_ "pe-trigger", makeAttributes "data-on:click" "@get('/sse/patch-elements')"] "Trigger"
      div_ [id_ "pe-result"] "Waiting..."

    section_ [id_ "patch-signals-test", makeAttributes "data-signals:message" "'Waiting...'"] $ do
      h2_ "Patch Signals"
      button_ [id_ "ps-trigger", makeAttributes "data-on:click" "@get('/sse/patch-signals')"] "Trigger"
      div_ [id_ "ps-result", makeAttributes "data-text" "$message"] "Waiting..."

    section_ [id_ "execute-script-test"] $ do
      h2_ "Execute Script"
      button_ [id_ "es-trigger", makeAttributes "data-on:click" "@get('/sse/execute-script')"] "Trigger"
      div_ [id_ "es-result"] "Waiting..."

    section_ [id_ "read-signals-test", makeAttributes "data-signals:greeting" "'Hello from Browser'"] $ do
      h2_ "Read Signals"
      button_ [id_ "rs-trigger", makeAttributes "data-on:click" "@get('/sse/read-signals')"] "Trigger"
      div_ [id_ "rs-result"] "Waiting..."

    section_ [id_ "multiple-events-test"] $ do
      h2_ "Multiple Events"
      button_ [id_ "me-trigger", makeAttributes "data-on:click" "@get('/sse/multiple-events')"] "Trigger"
      div_ [id_ "me-result"] "Waiting..."

main :: IO ()
main = do
  putStrLn "e2e-server running on http://localhost:3113"
  Warp.run 3113 app
