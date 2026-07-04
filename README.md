<p align="center"><img width="150" height="150" src="https://data-star.dev/static/images/rocket-512x512.png"></p>

# Datastar Haskell SDK

[![Test](https://github.com/starfederation/datastar-haskell/actions/workflows/test.yml/badge.svg)](https://github.com/starfederation/datastar-haskell/actions/workflows/test.yml)

A Haskell implementation of the [Datastar](https://data-star.dev/) SDK for building real-time hypermedia applications with server-sent events (SSE).

Live examples: <https://hamalainen.dev>

## License

This package is licensed for free under the [MIT License](LICENSE).

## Design

The SDK is built on [WAI](https://github.com/yesodweb/wai) (Web Application
Interface), Haskell's standard interface for HTTP servers. This means it works
with any WAI-compatible server (Warp, etc.) and any framework built on WAI
(Yesod, Scotty, Servant, etc.) without framework-specific adapters.

Key design decisions:

- **No system libraries** -- the core library depends only on `aeson`,
`bytestring`, `http-types`, `text`, and `wai`. A machine with just GHC (e.g. a
fresh [ghcup](https://www.haskell.org/ghcup/) install) builds it; no
`apt-get`/`brew` needed. Compressors that link against C libraries live in
add-on packages (see [Compression](#compression)).
- **WAI streaming** -- SSE responses use WAI's native `responseStream`, giving
you a `ServerSentEventGenerator` callback with `sendPatchElements`,
`sendPatchSignals`, and `sendExecuteScript`.
- **No routing opinion** -- the SDK provides request helpers (`readSignals`,
`isDatastarRequest`) but doesn't impose a routing framework. The examples use
simple pattern matching on `(requestMethod, pathInfo)`.

## API Overview

```haskell
import Hypermedia.Datastar

-- Create an SSE response
sseResponse :: DatastarLogger -> (ServerSentEventGenerator -> IO ()) -> Response

-- Send events
sendPatchElements  :: ServerSentEventGenerator -> PatchElements  -> IO ()
sendPatchSignals   :: ServerSentEventGenerator -> PatchSignals   -> IO ()
sendExecuteScript  :: ServerSentEventGenerator -> ExecuteScript  -> IO ()

-- Read signals from a request (query string for GET, body for POST)
readSignals :: FromJSON a => Request -> IO (Either String a)
```

## Quick Start

Add `datastar-hs` to your `build-depends`, then:

```haskell
import Hypermedia.Datastar
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp

app :: Application
app req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["hello"]) -> do
      Right signals <- readSignals req
      respond $ sseResponse nullLogger $ \gen -> do
        sendPatchElements gen (patchElements "<div id=\"message\">Hello!</div>")
    _ ->
      respond $ responseLBS status404 [] "Not found"

main :: IO ()
main = Warp.run 3000 app
```

## Compression

SSE streams can be compressed by negotiating `Content-Encoding` against the
request's `Accept-Encoding`. The compressors live in add-on packages so that
the core `datastar-hs` has no system-library dependencies:

| Package | Encodings | System library |
|---|---|---|
| `datastar-hs-zlib` | `gzip`, `deflate` | zlib -- preinstalled on macOS; `zlib1g-dev` on Debian/Ubuntu, or build with the constraint `zlib +bundled-c-zlib` for no system library at all |
| `datastar-hs-brotli` | `br` | `brew install brotli` / `apt-get install libbrotli-dev pkg-config` |
| `datastar-hs-zstd` | `zstd` | `brew install zstd` / `apt-get install libzstd-dev` -- not yet on Hackage, see below |

Add one to `build-depends` and pass its compressors to `sseResponseWith`
(or `sseResponseWithStrategy`) in preference order:

```haskell
import Hypermedia.Datastar
import Hypermedia.Datastar.Compression.Zlib (deflate, gzip)  -- datastar-hs-zlib

respond $ sseResponseWith nullLogger [gzip, deflate] req $ \gen ->
  sendPatchElements gen (patchElements "<div id=\"message\">Hello!</div>")
```

If the client accepts none of the offered encodings, the stream is sent
uncompressed.

### Compression benchmarks

See [bench/Main.hs](bench/Main.hs) for some compression benchmarks.

[Brotli](https://en.wikipedia.org/wiki/Brotli) is outstanding especially when you have
a large blob with small changes.


```
=== Identical large grid every tick ===
  400 events, ~130.7 KB uncompressed per fragment

  none   :      51.1 MB
  gzip   :       4.0 MB  (   12.8x vs none)
  brotli :       9.0 KB  ( 5779.1x vs none)
  zstd   :      13.6 KB  ( 3854.3x vs none)

=== Fat update: large grid, only the caption changes each tick ===
  400 events, ~130.7 KB uncompressed per fragment

  none   :      51.1 MB
  gzip   :       4.0 MB  (   12.7x vs none)
  brotli :       9.9 KB  ( 5265.5x vs none)
  zstd   :      19.4 KB  ( 2697.3x vs none)

=== Small update: tiny clock div each tick ===
  400 events, ~23 B uncompressed per fragment

  none   :      28.4 KB
  gzip   :       4.4 KB  (    6.5x vs none)
  brotli :       4.8 KB  (    6.0x vs none)
  zstd   :       5.2 KB  (    5.5x vs none)
```

### zstd upstream package

We [added support for flushStream](https://github.com/starfederation/datastar-haskell/issues/3) to
hs-zstd; until we get a new release on hackage, we are pinning the github source using `cabal.project`.

### zstd window size

The zstd compressor uses `ZSTD_initCStream` which sets
the compression level but not the window size, so you get zstd's default
window rather than a large one which would be optimal for "fat updates".

The Haskell wrapper for zstd exposes [compressStream](https://hackage.haskell.org/package/zstd-0.1.3.0/docs/Codec-Compression-Zstd-Base.html#v:compressStream)
but I think we need `ZSTD_compressStream2` to set parameters in `ZSTD_CCTx`?

```C
size_t ZSTD_compressStream2( ZSTD_CCtx* cctx,
                             ZSTD_outBuffer* output,
                             ZSTD_inBuffer* input,
                             ZSTD_EndDirective endOp);
```

<https://facebook.github.io/zstd/zstd_manual.html>:

    Behaves about the same as ZSTD_compressStream, with additional control on end directive.
    - Compression parameters are pushed into CCtx before starting compression, using ZSTD_CCtx_set*()

