# Revision history for datastar-hs-brotli

## 1.0.1.0 -- 2026-11-11

* Change `defaultBrotliParams` (and so `brotli`) to use less memory per open
  stream: the window drops from 24 bits (16 MB) to 22 bits (4 MB), and the
  quality goes from 5 to 6. The mode stays `CompressionModeText`. Use
  `brotliWith` to choose other settings
  ([#10](https://github.com/starfederation/datastar-haskell/issues/10)).

## 1.0.0.0 -- 2026-07-04

* First release. `Hypermedia.Datastar.Compression.Brotli`, split out of
  `datastar-hs` 1.0.x so the core package has no system-library dependencies.
