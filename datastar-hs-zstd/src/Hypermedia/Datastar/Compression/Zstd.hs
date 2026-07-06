{- |
A 'Compressor' that compresses an SSE stream with zstd (@Content-Encoding: zstd@),
driving libzstd's streaming API directly so each event can be flushed.
-}
module Hypermedia.Datastar.Compression.Zstd
  ( zstd
  , zstdWith
  , defaultZstdLevel
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Word (Word8)

import Foreign.ForeignPtr
  ( ForeignPtr
  , finalizeForeignPtr
  , mallocForeignPtrBytes
  , newForeignPtr
  , withForeignPtr
  )
import Foreign.Marshal.Alloc (finalizerFree, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)

import Codec.Compression.Zstd.FFI
  ( Buffer (..)
  , In
  , Out
  , compressStream
  , createCStream
  , cstreamOutSize
  , endStream
  , flushStream
  , getErrorName
  , initCStream
  , isError
  , p_freeCStream
  )

import Hypermedia.Datastar.WAI (Compressor (..))

newtype ZstdError = ZstdError String
  deriving (Show)

instance Exception ZstdError

-- | zstd's default compression level (3).
defaultZstdLevel :: Int
defaultZstdLevel = 3

-- | A zstd 'Compressor' (@Content-Encoding: zstd@) at 'defaultZstdLevel'.
zstd :: Compressor
zstd = zstdWith defaultZstdLevel

-- | A zstd 'Compressor' at an explicit compression level (1–22).
zstdWith :: Int -> Compressor
zstdWith level =
  Compressor
    { compressorEncoding = "zstd"
    , compressorWrap = \rawWrite rawFlush -> do
        let out = rawWrite . BSB.byteString
            outSize = fromIntegral cstreamOutSize :: Int

        csPtr <- createCStream
        when (csPtr == nullPtr) $ throwIO (ZstdError "ZSTD_createCStream returned NULL")
        initRet <- initCStream csPtr (fromIntegral level)
        when (isError initRet) $
          throwIO (ZstdError ("ZSTD_initCStream: " <> getErrorName initRet))
        csFp <- newForeignPtr p_freeCStream csPtr

        inFp <- newForeignPtr finalizerFree =<< (malloc :: IO (Ptr (Buffer In)))
        outFp <- newForeignPtr finalizerFree =<< (malloc :: IO (Ptr (Buffer Out)))
        scratchFp <- mallocForeignPtrBytes outSize :: IO (ForeignPtr Word8)

        let
          drainOut outBuf scratch = do
            produced <- fromIntegral . bufPos <$> peek outBuf
            when (produced > 0) $
              out =<< BS.packCStringLen (castPtr scratch, produced)

          resetOut outBuf scratch =
            poke outBuf (Buffer scratch (fromIntegral outSize) 0)

          checkRet ctx ret =
            when (isError ret) $ throwIO (ZstdError (ctx <> ": " <> getErrorName ret))

          write builder =
            let bs = BL.toStrict (BSB.toLazyByteString builder)
             in unless (BS.null bs) $
                  BU.unsafeUseAsCStringLen bs $ \(srcPtr, len) ->
                    withForeignPtr csFp $ \cs ->
                      withForeignPtr inFp $ \inBuf ->
                        withForeignPtr outFp $ \outBuf ->
                          withForeignPtr scratchFp $ \scratch -> do
                            poke inBuf (Buffer (castPtr srcPtr) (fromIntegral len) 0)
                            let loop = do
                                  resetOut outBuf scratch
                                  checkRet "ZSTD_compressStream" =<< compressStream cs outBuf inBuf
                                  drainOut outBuf scratch
                                  consumed <- fromIntegral . bufPos <$> peek inBuf
                                  when (consumed < len) loop
                            loop

          pump name op =
            withForeignPtr csFp $ \cs ->
              withForeignPtr outFp $ \outBuf ->
                withForeignPtr scratchFp $ \scratch -> do
                  let loop = do
                        resetOut outBuf scratch
                        remaining <- op cs outBuf
                        checkRet name remaining
                        drainOut outBuf scratch
                        when (remaining > 0) loop
                  loop

          flush = pump "ZSTD_flushStream" flushStream >> rawFlush

          finish = do
            pump "ZSTD_endStream" endStream
            finalizeForeignPtr csFp
            rawFlush

        pure (write, flush, finish)
    }
