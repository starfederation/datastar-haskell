{- |
A size benchmark for Datastar SSE compression.

It sends one @PatchElements@ event per "tick" over a single SSE connection and
measures the total bytes that go on the wire, uncompressed vs. gzip vs. brotli —
using the real SDK compressors and streaming path.

Run: @cabal run bench -- [numEvents] [tableRows]@ (defaults: 400 events, 2000 rows).
-}
module Main (main) where

import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (getArgs)
import Text.Printf (printf)

import Network.Wai (defaultRequest)
import Network.Wai.Internal (Response (..))

import Hypermedia.Datastar
import Hypermedia.Datastar.Compression.Brotli (brotli)
import Hypermedia.Datastar.Compression.Zlib (gzip)
import Hypermedia.Datastar.Compression.Zstd (zstd)

wireBytes :: Response -> IO Int
wireBytes (ResponseStream _ _ body) = do
  nrBytes <- newMVar 0

  let chunkLen = fromIntegral . BL.length . BSB.toLazyByteString

  body
    (\chunk -> modifyMVar_ nrBytes $ \nr -> return $ nr + chunkLen chunk)
    (pure ())

  readMVar nrBytes
wireBytes _ = error "expected a streaming response"

sendAll :: [Text] -> ServerSentEventGenerator -> IO ()
sendAll frags gen = forM_ frags (sendPatchElements gen . patchElements)

rawBytes :: [Text] -> IO Int
rawBytes frags = wireBytes (sseResponse nullLogger (sendAll frags))

compressedBytes :: Compressor -> [Text] -> IO Int
compressedBytes c frags =
  wireBytes (sseResponseWithStrategy Forced nullLogger [c] defaultRequest (sendAll frags))

bench :: String -> [Text] -> IO ()
bench name frags = do
  let n = length frags
      perEvent = if n == 0 then 0 else T.length (head frags)
  rawN <- rawBytes frags
  gzN <- compressedBytes gzip frags
  brN <- compressedBytes brotli frags

  printf "\n=== %s ===\n" name
  printf "  %d events, ~%s uncompressed per fragment\n\n" n (human perEvent)
  printf "  none   : %12s\n" (human rawN)
  printf "  gzip   : %12s  (%7.1fx vs none)\n" (human gzN) (ratio rawN gzN)
  printf "  brotli : %12s  (%7.1fx vs none)\n" (human brN) (ratio rawN brN)
  zstdN <- compressedBytes zstd frags
  printf "  zstd   : %12s  (%7.1fx vs none)\n" (human zstdN) (ratio rawN zstdN)

ratio :: Int -> Int -> Double
ratio a b = if b == 0 then 0 else fromIntegral a / fromIntegral b

human :: Int -> String
human n
  | n >= 1024 * 1024 = printf "%.1f MB" (fromIntegral n / (1024 * 1024) :: Double)
  | n >= 1024 = printf "%.1f KB" (fromIntegral n / 1024 :: Double)
  | otherwise = printf "%d B" n

tshow :: Int -> Text
tshow = T.pack . show

-- A self-similar table row (lots of intra-fragment repetition).
gridRow :: Int -> Text
gridRow i =
  "<tr><td>" <> tshow i <> "</td><td>item-" <> tshow i <> "</td><td>active</td><td>0.00</td></tr>"

-- A large grid (>32 KB for a couple thousand rows), optionally stamped with a
-- per-tick caption so only a tiny part changes between events.
grid :: Int -> Maybe Int -> Text
grid rows mtick =
  "<table id=\"grid\">"
    <> maybe "" (\t -> "<caption>updated " <> tshow t <> "</caption>") mtick
    <> T.concat (map gridRow [1 .. rows])
    <> "</table>"

clock :: Int -> Text
clock tick = "<div id=\"clock\">" <> tshow tick <> "</div>"

main :: IO ()
main = do
  args <- getArgs
  let n = case args of (a : _) -> read a; _ -> 400
      rows = case args of (_ : b : _) -> read b; _ -> 2000

  putStrLn "Datastar SSE compression benchmark"

  bench
    "Identical large grid every tick"
    (replicate n (grid rows Nothing))

  bench
    "Fat update: large grid, only the caption changes each tick"
    (map (grid rows . Just) [1 .. n])

  bench
    "Small update: tiny clock div each tick"
    (map clock [1 .. n])
