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

- **Minimal dependencies** -- the library depends only on `aeson`, `bytestring`,
`http-types`, `text`, and `wai`.
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
