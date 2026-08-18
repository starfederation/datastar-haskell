# Changelog

## 1.1.0.1

All changes contributed by [@sectore](https://github.com/sectore) — thanks!

* Fix double URL-decoding of the `datastar` query parameter in `readSignals`
  (GET/DELETE requests). `WAI.queryString` is already decoded, so the extra
  decode corrupted signal values containing literal `+` or `%` characters
  ([#8](https://github.com/starfederation/datastar-haskell/pull/8))
* Fix GHC warnings in the add-on packages' test suites
  ([#6](https://github.com/starfederation/datastar-haskell/pull/6))
* Add a `haskell-flake` based Nix flake for development
  ([#7](https://github.com/starfederation/datastar-haskell/pull/7))

## 1.1.0.0

* **Breaking**: the compressor modules moved to add-on packages so that the
  core `datastar-hs` has no system-library dependencies.
  GHC builds it.
  * `Hypermedia.Datastar.Compression.Zlib` (`gzip`, `deflate`) →
    [`datastar-hs-zlib`](https://hackage.haskell.org/package/datastar-hs-zlib)
  * `Hypermedia.Datastar.Compression.Brotli` (`brotli`) →
    [`datastar-hs-brotli`](https://hackage.haskell.org/package/datastar-hs-brotli)
  * `Hypermedia.Datastar.Compression.Zstd` (`zstd`) → `datastar-hs-zstd`
    (not yet on Hackage; the `zstd` cabal flag is gone — build the add-on
    from this repository instead)
* `Compressor`, `CompressionStrategy`, `sseResponseWith`, and
  `sseResponseWithStrategy` are unchanged and stay in the core.

## 1.0.2.1

* Fix the `zstd` ratio in the `compression-bench` output (it reported brotli's
  ratio by mistake) and correct the benchmark numbers in the README. No library
  changes.

## 1.0.2.0

* Add SSE response compression with `Content-Encoding` negotiation:
  * `sseResponseWith` and `sseResponseWithStrategy`, plus `Compressor` and
    `CompressionStrategy` (`ServerPriority`, `ClientPriority`, `Forced`) — all
    re-exported from `Hypermedia.Datastar`
  * `Hypermedia.Datastar.Compression.Brotli` (`brotli`)
  * `Hypermedia.Datastar.Compression.Zlib` (`gzip`, `deflate`)
  * `Hypermedia.Datastar.Compression.Zstd` (`zstd`) — opt-in behind the `zstd`
    cabal flag (off by default), as it needs an unreleased `hs-zstd`
    ([#3](https://github.com/starfederation/datastar-haskell/issues/3))
* Add a `compression-bench` executable for measuring compression ratios
* Enable stricter GHC warnings (`-Wall` and friends) across all components

## 1.0.1.1

* Revert experimental `examples` sub-library introduced in 1.0.1.0 — it broke Hackage doc uploads. Examples remain in the repo as runnable executables.

## 1.0.1.0

* Add `Hypermedia.Datastar.Attributes` with typed helpers for Datastar `data-*` attributes

## 1.0.0.0

* Bump to Datastar 1.0.0
* Handle DELETE method in `ReadSignals`

## 0.1.0.2

* Bump to Datastar RC.8
* Add end-to-end testing with Playwright

## 0.1.0.1

* Add example apps: hello-world (warp/servant/channel), activity-feed, heap-view
* Add DatastarLogger to sseResponse for configurable logging
* Add ExecuteScript and PatchSignals test specs
* Move repo to starfederation/datastar-haskell
* Add hie.yaml for HLS multi-component support

## 0.1.0.0

* Initial release.
