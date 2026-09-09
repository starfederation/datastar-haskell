# Revision history for datastar-hs-zstd

## 1.0.0.0 -- 2026-09-09

* First release. `Hypermedia.Datastar.Compression.Zstd`, split out of
  `datastar-hs` 1.0.x (where it was behind the `zstd` cabal flag).
* Requires `zstd >= 0.1.4`, the first Hackage release with the streaming
  `flushStream` FFI binding
  ([#3](https://github.com/starfederation/datastar-haskell/issues/3)). The
  `zstd` package bundles the zstd C sources by default, so no system library
  is needed.
