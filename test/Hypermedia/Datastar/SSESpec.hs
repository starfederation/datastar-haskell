module Hypermedia.Datastar.SSESpec where

import Data.Text qualified as T
import Test.Hspec

import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.IORef
import Data.List (isInfixOf)

import Hypermedia.Datastar
import Hypermedia.Datastar.PatchElements
import Hypermedia.Datastar.PatchSignals
import Hypermedia.Datastar.Types
import Hypermedia.Datastar.WAI (renderEvent)
import Network.Wai.Internal (Response (..))

render :: DatastarEvent -> String
render = LBS.unpack . BSB.toLazyByteString . renderEvent

-- | Consume a streaming WAI response and return the full body as a string.
consumeStreamingResponse :: Response -> IO String
consumeStreamingResponse (ResponseStream _status _headers body) = do
  chunksRef <- newIORef []
  body
    (\chunk -> modifyIORef' chunksRef (chunk :))
    (pure ())
  chunks <- readIORef chunksRef
  pure $ LBS.unpack $ BSB.toLazyByteString $ mconcat $ reverse chunks
consumeStreamingResponse _ = error "Expected a streaming response"

spec :: Spec
spec = do
  describe "Hypermedia.Datastar.SSE.renderEvent" $ do
    it "renders a simple event" $ do
      let event =
            DatastarEvent
              { eventType = EventPatchElements
              , eventId = Nothing
              , retry = defaultRetryDuration
              , dataLines = ["elements <div id=\"feed\"><span>1</span></div>"]
              }
      render event
        `shouldBe` "event: datastar-patch-elements\n\
                   \data: elements <div id=\"feed\"><span>1</span></div>\n\
                   \\n"

  describe "SSE streaming" $ do
    it "sends multiple events over a single SSE connection" $ do
      let e0 = "<div id=\"a\">1</div>"
          e1 = "<div id=\"b\">2</div>"
          e2 = "{\"count\": 42}"

          resp = sseResponse $ \gen -> do
            sendPatchElements gen (patchElements e0)
            sendPatchElements gen (patchElements e1)
            sendPatchSignals gen (patchSignals e2)

      body <- consumeStreamingResponse resp

      let s0 = "elements " ++ T.unpack e0
          s1 = "elements " ++ T.unpack e1
          s2 = "signals " ++ T.unpack e2

      -- All three events are present in the single response body
      length (filter ("event: datastar-patch-elements" `isInfixOf`) (lines body))
        `shouldBe` 2
      length (filter ("event: datastar-patch-signals" `isInfixOf`) (lines body))
        `shouldBe` 1

      -- The actual data is there too
      body `shouldSatisfy` isInfixOf s0
      body `shouldSatisfy` isInfixOf s1
      body `shouldSatisfy` isInfixOf s2

      -- Events arrive in the order they were sent
      let eventLines = filter (\l -> "event: " `isInfixOf` l || "data: " `isInfixOf` l) (lines body)
          indexOf needle = length $ takeWhile (not . isInfixOf needle) eventLines
      indexOf s0 `shouldSatisfy` (< indexOf s1)
      indexOf s1 `shouldSatisfy` (< indexOf s2)
