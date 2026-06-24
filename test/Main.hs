{-# LANGUAGE CPP #-}

module Main where

import Test.Hspec

import Hypermedia.Datastar.Compression.BrotliSpec qualified
import Hypermedia.Datastar.Compression.ZlibSpec qualified
#ifdef ZSTD
import Hypermedia.Datastar.Compression.ZstdSpec qualified
#endif
import Hypermedia.Datastar.ExecuteScriptSpec qualified
import Hypermedia.Datastar.PatchElementsSpec qualified
import Hypermedia.Datastar.PatchSignalsSpec qualified
import Hypermedia.Datastar.SSESpec qualified

main :: IO ()
main = hspec $ do
  Hypermedia.Datastar.Compression.BrotliSpec.spec
  Hypermedia.Datastar.Compression.ZlibSpec.spec
#ifdef ZSTD
  Hypermedia.Datastar.Compression.ZstdSpec.spec
#endif
  Hypermedia.Datastar.ExecuteScriptSpec.spec
  Hypermedia.Datastar.PatchElementsSpec.spec
  Hypermedia.Datastar.PatchSignalsSpec.spec
  Hypermedia.Datastar.SSESpec.spec
