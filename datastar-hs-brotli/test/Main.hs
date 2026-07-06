module Main where

import Test.Hspec

import Hypermedia.Datastar.Compression.BrotliSpec qualified

main :: IO ()
main = hspec Hypermedia.Datastar.Compression.BrotliSpec.spec
