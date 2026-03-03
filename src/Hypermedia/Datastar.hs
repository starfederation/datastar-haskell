-- |
-- Module      : Hypermedia.Datastar
-- Description : Haskell SDK for building real-time hypermedia apps with Datastar
--
-- <https://data-star.dev/ Datastar> is a hypermedia framework: instead of
-- building a JSON API and a JavaScript SPA, you write HTML on the server and
-- let Datastar handle the interactivity. The browser sends requests, the
-- server holds the connection open as a server-sent event (SSE) stream, and
-- pushes HTML fragments, signal updates, or scripts back to the browser as
-- things change.
--
-- This SDK provides the server-side Haskell API. It builds on
-- <https://hackage.haskell.org/package/wai WAI> so it works with Warp, Scotty,
-- Servant, Yesod, or any other WAI-compatible framework.
--
-- === Minimal example
--
-- @
-- import Hypermedia.Datastar
-- import Network.Wai (Application, responseLBS, requestMethod, pathInfo)
-- import Network.Wai.Handler.Warp qualified as Warp
-- import Network.HTTP.Types (status404)
--
-- app :: Application
-- app req respond =
--   case (requestMethod req, pathInfo req) of
--     (\"GET\", [\"hello\"]) ->
--       respond $ sseResponse $ \\gen ->
--         sendPatchElements gen (patchElements \"\<div id=\\\"msg\\\"\>Hello!\<\/div\>\")
--     _ ->
--       respond $ responseLBS status404 [] \"Not found\"
--
-- main :: IO ()
-- main = Warp.run 3000 app
-- @
--
-- === Module guide
--
-- * "Hypermedia.Datastar.PatchElements" — send HTML to morph into the DOM
-- * "Hypermedia.Datastar.PatchSignals" — update the browser's reactive signals
-- * "Hypermedia.Datastar.ExecuteScript" — run JavaScript in the browser
-- * "Hypermedia.Datastar.WAI" — SSE streaming, signal decoding, request helpers
-- * "Hypermedia.Datastar.Types" — protocol types and defaults
--
-- === Further reading
--
-- * <https://data-star.dev/ Datastar homepage> — guides, reference, and examples
-- * <https://github.com/carlohamalainen/datastar-hs-examples Examples repository> — full working Haskell examples
-- * <https://cljdoc.org/d/dev.data-star.clojure/http-kit/1.0.0-RC7/doc/sdk-docs/using-datastar Clojure SDK docs> — excellent Datastar walkthrough that applies across SDKs
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
  )
where

import Hypermedia.Datastar.ExecuteScript (ExecuteScript (..), executeScript)
import Hypermedia.Datastar.PatchElements (PatchElements (..), patchElements, removeElements)
import Hypermedia.Datastar.PatchSignals (PatchSignals (..), patchSignals)
import Hypermedia.Datastar.Types (ElementNamespace (..), ElementPatchMode (..), EventType (..))
import Hypermedia.Datastar.WAI
  ( ServerSentEventGenerator
  , isDatastarRequest
  , readSignals
  , sendExecuteScript
  , sendPatchElements
  , sendPatchSignals
  , sseResponse
  )
