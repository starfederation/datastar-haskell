module Main where

import Test.Hspec

import Hypermedia.Datastar.PatchElementsSpec qualified
import Hypermedia.Datastar.SSESpec qualified 

main :: IO ()
main = hspec $ do
  Hypermedia.Datastar.PatchElementsSpec.spec
  Hypermedia.Datastar.SSESpec.spec