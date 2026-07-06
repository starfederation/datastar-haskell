module Main where

import Test.Hspec

import Hypermedia.Datastar.Compression.ZstdSpec qualified

main :: IO ()
main = hspec Hypermedia.Datastar.Compression.ZstdSpec.spec
