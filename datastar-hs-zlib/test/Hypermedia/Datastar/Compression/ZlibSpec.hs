module Hypermedia.Datastar.Compression.ZlibSpec (spec) where

import Test.Hspec

import Control.Monad (forM_)

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef

import Network.Wai (defaultRequest, requestHeaders)
import Network.Wai.Internal (Response (..))

import Hypermedia.Datastar
import Hypermedia.Datastar.Compression.Zlib (deflate, gzip)
import Hypermedia.Datastar.Logger (nullLogger)
import Hypermedia.Datastar.WAI (compressorWrap)

{- | Drive a streaming WAI response to completion, returning its response headers
and the full raw body.
-}
runStream (ResponseStream _status headers body) = do
  ref <- newIORef mempty
  body (\chunk -> modifyIORef' ref (<> chunk)) (pure ())
  bytes <- BSB.toLazyByteString <$> readIORef ref
  pure (headers, bytes)
runStream _ = error "expected a streaming response"

spec :: Spec
spec = describe "Hypermedia.Datastar.Compression.Zlib" $ do
  let sendEvents gen = do
        sendPatchElements gen (patchElements "<div id=\"a\">1</div>")
        sendPatchElements gen (patchElements "<div id=\"b\">2</div>")
        sendPatchSignals gen (patchSignals "{\"count\":42}")
      withAccept enc = defaultRequest{requestHeaders = [("Accept-Encoding", enc)]}

      -- (encoding token, compressor, matching decompressor)
      codecs :: [(BS.ByteString, Compressor, BL.ByteString -> BL.ByteString)]
      codecs =
        [ ("gzip", gzip, GZip.decompress)
        , ("deflate", deflate, Zlib.decompress)
        ]

  forM_ codecs $ \(enc, comp, decompress) -> do
    let name = BS8.unpack enc

    it (name <> ": round-trips to the uncompressed stream") $ do
      (_, reference) <- runStream (sseResponse nullLogger sendEvents)
      (headers, compressed) <-
        runStream (sseResponseWith nullLogger [comp] (withAccept enc) sendEvents)
      headers `shouldSatisfy` elem ("Content-Encoding", enc)
      decompress compressed `shouldBe` reference

    it (name <> ": emits output incrementally on flush, not buffered until finish") $ do
      ref <- newIORef mempty
      let rawWrite c = modifyIORef' ref (<> c)
          sizeSoFar = fromIntegral . BL.length . BSB.toLazyByteString <$> readIORef ref
      (write, flush, finish) <- compressorWrap comp rawWrite (pure ())

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

      -- And the accumulated stream is still a valid, complete stream.
      whole <- BSB.toLazyByteString <$> readIORef ref
      decompress whole
        `shouldBe` "event: datastar-patch-elements\ndata: elements <div>1</div>\n\n\
                   \event: datastar-patch-elements\ndata: elements <div>2</div>\n\n"

  it "declines when the client accepts neither" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, body) <-
      runStream (sseResponseWith nullLogger [gzip, deflate] (withAccept "br") sendEvents)
    filter ((== "Content-Encoding") . fst) headers `shouldBe` []
    body `shouldBe` reference

  -- With several server compressors offered and a client that accepts all of
  -- them, ServerPriority (the default for sseResponseWith) picks the *server's*
  -- first — so list order, not header order, decides. Verified end-to-end via
  -- the chosen Content-Encoding and that the matching codec decompresses it.
  it "picks the server's first compressor when the client accepts several (gzip first)" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, compressed) <-
      runStream (sseResponseWith nullLogger [gzip, deflate] (withAccept "deflate, gzip") sendEvents)
    headers `shouldSatisfy` elem ("Content-Encoding", "gzip")
    GZip.decompress compressed `shouldBe` reference

  it "picks the server's first compressor when the client accepts several (deflate first)" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, compressed) <-
      runStream (sseResponseWith nullLogger [deflate, gzip] (withAccept "deflate, gzip") sendEvents)
    headers `shouldSatisfy` elem ("Content-Encoding", "deflate")
    Zlib.decompress compressed `shouldBe` reference
