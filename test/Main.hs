module Main where

import Test.Hspec

import Hypermedia.Datastar qualified
import Hypermedia.Datastar.ExecuteScriptSpec qualified
import Hypermedia.Datastar.PatchElementsSpec qualified
import Hypermedia.Datastar.PatchSignalsSpec qualified
import Hypermedia.Datastar.SSESpec qualified

main :: IO ()
main = hspec $ do
  Hypermedia.Datastar.ExecuteScriptSpec.spec
  Hypermedia.Datastar.PatchElementsSpec.spec
  Hypermedia.Datastar.PatchSignalsSpec.spec
  Hypermedia.Datastar.SSESpec.spec
