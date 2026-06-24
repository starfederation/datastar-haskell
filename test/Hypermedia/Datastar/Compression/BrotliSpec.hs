module Hypermedia.Datastar.Compression.BrotliSpec (spec) where

import Test.Hspec

import Codec.Compression.Brotli qualified as B
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BL
import Data.IORef

import Network.Wai (defaultRequest, requestHeaders)
import Network.Wai.Internal (Response (..))

import Hypermedia.Datastar
import Hypermedia.Datastar.Compression.Brotli (brotli)
import Hypermedia.Datastar.Logger (nullLogger)
import Hypermedia.Datastar.WAI (Compressor (..), compressorWrap, negotiateWith)

-- | Drive a streaming WAI response to completion, returning its response headers
-- and the full raw body.
runStream (ResponseStream _status headers body) = do
  ref <- newIORef mempty
  body (\chunk -> modifyIORef' ref (<> chunk)) (pure ())
  bytes <- BSB.toLazyByteString <$> readIORef ref
  pure (headers, bytes)
runStream _ = error "expected a streaming response"

-- | A compressor that only carries an encoding token, for negotiation tests.
-- Its 'compressorWrap' is never invoked by 'negotiateWith'.
fakeCompressor :: BL.ByteString -> Compressor
fakeCompressor enc = Compressor (BL.toStrict enc) (\_ _ -> error "unused")

spec :: Spec
spec = describe "Hypermedia.Datastar.Compression.Brotli" $ do
  let sendEvents gen = do
        sendPatchElements gen (patchElements "<div id=\"a\">1</div>")
        sendPatchElements gen (patchElements "<div id=\"b\">2</div>")
        sendPatchSignals gen (patchSignals "{\"count\":42}")
      withAccept enc = defaultRequest {requestHeaders = [("Accept-Encoding", enc)]}

  it "round-trips: the br stream decompresses to the uncompressed stream" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, compressed) <-
      runStream (sseResponseWith nullLogger [brotli] (withAccept "br") sendEvents)

    headers `shouldSatisfy` elem ("Content-Encoding", "br")
    B.decompress compressed `shouldBe` reference

  it "declines compression when the client does not accept br" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, body) <-
      runStream (sseResponseWith nullLogger [brotli] (withAccept "gzip") sendEvents)

    filter ((== "Content-Encoding") . fst) headers `shouldBe` []
    body `shouldBe` reference

  it "emits output incrementally on flush, not buffered until finish" $ do
    ref <- newIORef mempty
    let rawWrite c = modifyIORef' ref (<> c)
        sizeSoFar = fromIntegral . BL.length . BSB.toLazyByteString <$> readIORef ref
    (write, flush, finish) <- compressorWrap brotli rawWrite (pure ())

    write (BSB.byteString "event: datastar-patch-elements\ndata: elements <div>1</div>\n\n")
    flush
    afterFirst <- sizeSoFar

    write (BSB.byteString "event: datastar-patch-elements\ndata: elements <div>2</div>\n\n")
    flush
    afterSecond <- sizeSoFar

    finish
    afterFinish <- sizeSoFar

    -- Each flush must push bytes onto the wire; a compressor that buffered
    -- everything until finish would leave afterFirst == afterSecond == 0.
    afterFirst `shouldSatisfy` (> (0 :: Int))
    afterSecond `shouldSatisfy` (> afterFirst)
    afterFinish `shouldSatisfy` (>= afterSecond)

    -- And the accumulated stream is still a valid, complete brotli stream.
    whole <- BSB.toLazyByteString <$> readIORef ref
    B.decompress whole
      `shouldBe` "event: datastar-patch-elements\ndata: elements <div>1</div>\n\n\
                 \event: datastar-patch-elements\ndata: elements <div>2</div>\n\n"

  describe "negotiateWith" $ do
    let br = fakeCompressor "br"
        gz = fakeCompressor "gzip"
        enc = fmap compressorEncoding
        req h = defaultRequest {requestHeaders = [("Accept-Encoding", h)]}

    it "ServerPriority picks the first server compressor the client accepts" $
      enc (negotiateWith ServerPriority [br, gz] (req "gzip, br")) `shouldBe` Just "br"

    it "ClientPriority picks the client's most-preferred offered encoding" $
      enc (negotiateWith ClientPriority [br, gz] (req "gzip, br")) `shouldBe` Just "gzip"

    it "Forced ignores Accept-Encoding entirely" $
      enc (negotiateWith Forced [br, gz] (req "identity")) `shouldBe` Just "br"

    it "ignores q-value params and whitespace around each token" $
      enc (negotiateWith ClientPriority [br, gz] (req "  gzip;q=0.1 , br ")) `shouldBe` Just "gzip"

    it "returns Nothing when no server encoding is accepted" $
      enc (negotiateWith ServerPriority [br, gz] (req "deflate")) `shouldBe` Nothing

    it "returns Nothing when the Accept-Encoding header is absent" $
      enc (negotiateWith ServerPriority [br, gz] defaultRequest) `shouldBe` Nothing
