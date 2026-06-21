{- |
Module      : Hypermedia.Datastar
Description : Haskell SDK for building real-time hypermedia apps with Datastar

<https://data-star.dev/ Datastar> is a hypermedia framework: instead of
building a JSON API and a JavaScript SPA, you write HTML on the server and
let Datastar handle the interactivity. The browser sends requests, the
server holds the connection open as a server-sent event (SSE) stream, and
pushes HTML fragments, signal updates, or scripts back to the browser as
things change.

This SDK provides the server-side Haskell API. It builds on
<https://hackage.haskell.org/package/wai WAI> so it works with Warp, Scotty,
Servant, Yesod, or any other WAI-compatible framework.

=== Minimal example

@
import Hypermedia.Datastar
import Network.Wai (Application, responseLBS, requestMethod, pathInfo)
import Network.Wai.Handler.Warp qualified as Warp
import Network.HTTP.Types (status404)

app :: Application
app req respond =
  case (requestMethod req, pathInfo req) of
    (\"GET\", [\"hello\"]) ->
      respond $ sseResponse $ \\gen ->
        sendPatchElements gen (patchElements \"\<div id=\\\"msg\\\"\>Hello!\<\/div\>\")
    _ ->
      respond $ responseLBS status404 [] \"Not found\"

main :: IO ()
main = Warp.run 3000 app
@

=== With compression

Use 'sseResponseWith' to negotiate a @Content-Encoding@ from the request's
@Accept-Encoding@ header. Pass the compressors you want to offer, in
preference order — the first one the client accepts wins:

@
import Hypermedia.Datastar
import Hypermedia.Datastar.Compression.Brotli (brotli)
import Hypermedia.Datastar.Compression.Zlib (deflate, gzip)

app req respond =
  respond $ sseResponseWith nullLogger [brotli, gzip, deflate] req $ \\gen ->
    sendPatchElements gen (patchElements \"\<div id=\\\"msg\\\"\>Hello!\<\/div\>\")
@

If the client accepts none of them, the stream is sent uncompressed. Use
'sseResponseWithStrategy' to control negotiation with a 'CompressionStrategy'
(e.g. prefer the client's order, or force a single encoding).

=== Module guide

* "Hypermedia.Datastar.PatchElements" — send HTML to morph into the DOM
* "Hypermedia.Datastar.PatchSignals" — update the browser's reactive signals
* "Hypermedia.Datastar.ExecuteScript" — run JavaScript in the browser
* "Hypermedia.Datastar.WAI" — SSE streaming, signal decoding, request helpers
* "Hypermedia.Datastar.Compression.Brotli", "Hypermedia.Datastar.Compression.Zlib" — @Content-Encoding@ compressors
* "Hypermedia.Datastar.Types" — protocol types and defaults

=== Further reading

* <https://data-star.dev/ Datastar homepage> — guides, reference, and examples
* <https://cljdoc.org/d/dev.data-star.clojure/http-kit/1.0.0-RC7/doc/sdk-docs/using-datastar Clojure SDK docs> — excellent Datastar walkthrough that applies across SDKs
-}
module Hypermedia.Datastar
  ( -- * Types
    EventType (..)
  , ElementPatchMode (..)
  , ElementNamespace (..)

    -- * Patch Elements
  , PatchElements (..)
  , patchElements
  , removeElements

    -- * Patch Signals
  , PatchSignals (..)
  , patchSignals

    -- * Execute Script
  , ExecuteScript (..)
  , executeScript

    -- * WAI
  , ServerSentEventGenerator
  , sseResponse
  , sendPatchElements
  , sendPatchSignals
  , sendExecuteScript
  , readSignals
  , isDatastarRequest

    -- * Compression
    --
    -- | Negotiate @Content-Encoding@ for an SSE stream. Pass one or more
    -- compressors from "Hypermedia.Datastar.Compression.Brotli" or
    -- "Hypermedia.Datastar.Compression.Zlib". 'sseResponseWith' uses
    -- 'ServerPriority'; 'sseResponseWithStrategy' lets you choose.
  , Compressor
  , CompressionStrategy (..)
  , sseResponseWith
  , sseResponseWithStrategy

    -- * Logger
  , DatastarLogger
  , nullLogger
  , stderrLogger
  )
where

import Hypermedia.Datastar.ExecuteScript (ExecuteScript (..), executeScript)
import Hypermedia.Datastar.Logger (DatastarLogger, nullLogger, stderrLogger)
import Hypermedia.Datastar.PatchElements (PatchElements (..), patchElements, removeElements)
import Hypermedia.Datastar.PatchSignals (PatchSignals (..), patchSignals)
import Hypermedia.Datastar.Types (ElementNamespace (..), ElementPatchMode (..), EventType (..))
import Hypermedia.Datastar.WAI
  ( CompressionStrategy (..)
  , Compressor
  , ServerSentEventGenerator
  , isDatastarRequest
  , readSignals
  , sendExecuteScript
  , sendPatchElements
  , sendPatchSignals
  , sseResponse
  , sseResponseWith
  , sseResponseWithStrategy
  )
