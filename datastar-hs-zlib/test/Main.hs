module Main where

import Test.Hspec

import Hypermedia.Datastar.Compression.ZlibSpec qualified

main :: IO ()
main = hspec Hypermedia.Datastar.Compression.ZlibSpec.spec
