module Hypermedia.Datastar.Compression.Zlib
  ( gzip
  , gzipWith
  , deflate
  , deflateWith
  , defaultCompressionLevel
  )
where

import Control.Exception (throwIO)

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BL

import Data.Streaming.Zlib
  ( PopperRes (..)
  , WindowBits (..)
  , feedDeflate
  , finishDeflate
  , flushDeflate
  , initDeflate
  )

import Hypermedia.Datastar.WAI (Compressor (..))

defaultCompressionLevel :: Int
defaultCompressionLevel = 6

-- | A gzip 'Compressor' (@Content-Encoding: gzip@) at 'defaultCompressionLevel'.
gzip :: Compressor
gzip = gzipWith defaultCompressionLevel

-- | A gzip 'Compressor' at an explicit zlib level (0–9).
gzipWith :: Int -> Compressor
gzipWith level = zlibCompressor "gzip" level (WindowBits 31)

-- | A zlib/deflate 'Compressor' (@Content-Encoding: deflate@) at 'defaultCompressionLevel'.
deflate :: Compressor
deflate = deflateWith defaultCompressionLevel

-- | A zlib/deflate 'Compressor' at an explicit zlib level (0–9).
deflateWith :: Int -> Compressor
deflateWith level = zlibCompressor "deflate" level (WindowBits 15)

-- WindowBits 31 selects gzip framing, 15 selects zlib/deflate framing.
zlibCompressor :: BS.ByteString -> Int -> WindowBits -> Compressor
zlibCompressor enc level wbits = Compressor enc wrap
  where 
    wrap rawWrite rawFlush = do
        def <- initDeflate level wbits

        let drain popper = do
              res <- popper
              case res of
                PRDone -> pure ()
                PRNext bs -> rawWrite (BSB.byteString bs) >> drain popper
                PRError e -> throwIO e

            write builder = do
              popper <- feedDeflate def $ BL.toStrict $ BSB.toLazyByteString builder
              drain popper

            flush = drain (flushDeflate def) >> rawFlush

            finish = drain (finishDeflate def) >> rawFlush

        pure (write, flush, finish)
