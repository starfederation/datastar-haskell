module Hypermedia.Datastar.Compression.Brotli
  ( brotli
  , brotliWith
  , defaultBrotliParams
  )
where

import Codec.Compression.Brotli qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BL
import Control.Concurrent.MVar

import Hypermedia.Datastar.WAI (Compressor (..))

brotli :: Compressor
brotli = brotliWith defaultBrotliParams

brotliWith :: B.CompressParams -> Compressor
brotliWith params = Compressor "br" wrap
  where
    wrap rawWrite rawFlush = do
      csM <- B.compressIO params >>= newMVar

      let write builder = do
            let bs = BL.toStrict $ BSB.toLazyByteString builder
            if BS.null bs
              then pure ()
              else modifyMVar_ csM $ \case 
                     B.CompressInputRequired _ supply ->
                       supply bs >>= doRawWrites rawWrite
                     other -> pure other

          flush = do
            modifyMVar_ csM $ \case
              B.CompressInputRequired doFlush _ ->
                doFlush >>= doRawWrites rawWrite
              other -> pure other
            rawFlush

          finish = do
            modifyMVar_ csM $ \case
              B.CompressInputRequired _ supply ->
                supply BS.empty >>= doRawWrites rawWrite
              other -> pure other
            rawFlush

      return (write, flush, finish)

doRawWrites
  :: (BSB.Builder -> IO ())
  -> B.CompressStream IO
  -> IO (B.CompressStream IO)
doRawWrites rawWrite cs = case cs of
  B.CompressOutputAvailable chunk nextAction -> do
    rawWrite $ BSB.byteString chunk
    cs' <- nextAction
    doRawWrites rawWrite cs'
  _ -> pure cs

defaultBrotliParams :: B.CompressParams
defaultBrotliParams =
  B.defaultCompressParams
    { B.compressMode = B.CompressionModeText
    , B.compressLevel = B.CompressionLevel5
    , B.compressWindowSize = B.CompressionWindowBits24
    }

