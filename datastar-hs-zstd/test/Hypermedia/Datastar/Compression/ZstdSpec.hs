module Hypermedia.Datastar.Compression.ZstdSpec (spec) where

import Test.Hspec

import Codec.Compression.Zstd.Lazy qualified as Zstd
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BL
import Data.IORef

import Network.Wai (defaultRequest, requestHeaders)
import Network.Wai.Internal (Response (..))

import Hypermedia.Datastar
import Hypermedia.Datastar.Compression.Zlib (gzip)
import Hypermedia.Datastar.Compression.Zstd (zstd)
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
spec = describe "Hypermedia.Datastar.Compression.Zstd" $ do
  let sendEvents gen = do
        sendPatchElements gen (patchElements "<div id=\"a\">1</div>")
        sendPatchElements gen (patchElements "<div id=\"b\">2</div>")
        sendPatchSignals gen (patchSignals "{\"count\":42}")
      withAccept enc = defaultRequest{requestHeaders = [("Accept-Encoding", enc)]}

  it "round-trips to the uncompressed stream" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, compressed) <-
      runStream (sseResponseWith nullLogger [zstd] (withAccept "zstd") sendEvents)
    headers `shouldSatisfy` elem ("Content-Encoding", "zstd")
    Zstd.decompress compressed `shouldBe` reference

  it "declines when the client does not accept zstd" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, body) <-
      runStream (sseResponseWith nullLogger [zstd] (withAccept "gzip") sendEvents)
    filter ((== "Content-Encoding") . fst) headers `shouldBe` []
    body `shouldBe` reference

  it "is chosen ahead of gzip when offered first and both are accepted" $ do
    (_, reference) <- runStream (sseResponse nullLogger sendEvents)
    (headers, compressed) <-
      runStream (sseResponseWith nullLogger [zstd, gzip] (withAccept "gzip, zstd") sendEvents)
    headers `shouldSatisfy` elem ("Content-Encoding", "zstd")
    Zstd.decompress compressed `shouldBe` reference

  it "emits output incrementally on flush, not buffered until finish" $ do
    ref <- newIORef mempty
    let rawWrite c = modifyIORef' ref (<> c)
        sizeSoFar = fromIntegral . BL.length . BSB.toLazyByteString <$> readIORef ref
    (write, flush, finish) <- compressorWrap zstd rawWrite (pure ())

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

    whole <- BSB.toLazyByteString <$> readIORef ref
    Zstd.decompress whole
      `shouldBe` "event: datastar-patch-elements\ndata: elements <div>1</div>\n\n\
                 \event: datastar-patch-elements\ndata: elements <div>2</div>\n\n"
