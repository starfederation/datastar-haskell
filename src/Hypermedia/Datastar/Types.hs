-- |
-- Module      : Hypermedia.Datastar.Types
-- Description : Core types for the Datastar SSE protocol
--
-- This module defines the types that model the Datastar server-sent events
-- protocol. Most users won't import this module directly — the types are
-- re-exported from "Hypermedia.Datastar".
--
-- Default values for protocol fields ('defaultPatchMode', 'defaultRetryDuration',
-- etc.) follow the Datastar ADR specification at
-- <https://data-star.dev/reference/action_plugins>.
module Hypermedia.Datastar.Types where

import Data.Text (Text)

-- | The two SSE event types defined by the Datastar protocol.
--
-- Every event the server sends is one of these. 'EventPatchElements' covers
-- both DOM patching and script execution (the protocol encodes @executeScript@
-- as a special case of element patching). 'EventPatchSignals' updates the
-- browser's reactive signal store.
data EventType
  = -- | Sent as @datastar-patch-elements@ on the wire. Used by both
    -- 'Hypermedia.Datastar.PatchElements.PatchElements' and
    -- 'Hypermedia.Datastar.ExecuteScript.ExecuteScript'.
    EventPatchElements
  | -- | Sent as @datastar-patch-signals@ on the wire. Used by
    -- 'Hypermedia.Datastar.PatchSignals.PatchSignals'.
    EventPatchSignals
  deriving (Eq, Show)

eventTypeToText :: EventType -> Text
eventTypeToText EventPatchElements = "datastar-patch-elements"
eventTypeToText EventPatchSignals = "datastar-patch-signals"

-- | How the patched HTML should be applied to the DOM.
--
-- The default mode is 'Outer', which replaces the target element (matched by
-- its @id@ attribute) including the element itself. This works well with
-- Datastar's morphing algorithm, which preserves focus, scroll position, and
-- CSS transitions during the replacement.
data ElementPatchMode
  = -- | Replace the target element and its contents (the default).
    Outer
  | -- | Replace only the target element's children, keeping the element itself.
    Inner
  | -- | Remove the target element from the DOM entirely.
    Remove
  | -- | Replace the target element without morphing (a hard swap).
    Replace
  | -- | Insert the new content as the first child of the target element.
    Prepend
  | -- | Insert the new content as the last child of the target element.
    Append
  | -- | Insert the new content immediately before the target element.
    Before
  | -- | Insert the new content immediately after the target element.
    After
  deriving (Eq, Show)

patchModeToText :: ElementPatchMode -> Text
patchModeToText Outer = "outer"
patchModeToText Inner = "inner"
patchModeToText Remove = "remove"
patchModeToText Replace = "replace"
patchModeToText Prepend = "prepend"
patchModeToText Append = "append"
patchModeToText Before = "before"
patchModeToText After = "after"

-- | The XML namespace for the patched elements.
--
-- Almost all content uses 'HtmlNs' (the default). Use 'SvgNs' or 'MathmlNs'
-- when patching inline SVG or MathML elements so that Datastar creates them
-- in the correct namespace.
data ElementNamespace
  = -- | Standard HTML namespace (the default).
    HtmlNs
  | -- | SVG namespace — use when patching @\<svg\>@ content.
    SvgNs
  | -- | MathML namespace — use when patching @\<math\>@ content.
    MathmlNs
  deriving (Eq, Show)

namespaceToText :: ElementNamespace -> Text
namespaceToText HtmlNs = "html"
namespaceToText SvgNs = "svg"
namespaceToText MathmlNs = "mathml"

-- | Internal representation of a rendered SSE event.
--
-- Users don't construct these directly. Instead, use 'Hypermedia.Datastar.PatchElements.patchElements',
-- 'Hypermedia.Datastar.PatchSignals.patchSignals', or 'Hypermedia.Datastar.ExecuteScript.executeScript'
-- to build events, and 'Hypermedia.Datastar.WAI.sendPatchElements' (etc.) to send them.
data DatastarEvent = DatastarEvent
  { eventType :: EventType
  , eventId :: Maybe Text
  , retry :: Int
  , dataLines :: [Text]
  }

-- | Default SSE retry duration in milliseconds.
--
-- If the connection drops, the browser waits this long before reconnecting.
-- Per the Datastar ADR spec, the default is @1000@ ms.
defaultRetryDuration :: Int
defaultRetryDuration = 1000

-- | Default element patch mode: 'Outer'.
--
-- Replaces the target element (matched by @id@) and its contents using
-- Datastar's morphing algorithm.
defaultPatchMode :: ElementPatchMode
defaultPatchMode = Outer

-- | Default for the @useViewTransition@ flag: 'False'.
--
-- When 'True', Datastar wraps the DOM update in a
-- <https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API View Transition>,
-- enabling CSS-animated transitions between states.
defaultUseViewTransition :: Bool
defaultUseViewTransition = False

-- | Default for the @onlyIfMissing@ flag: 'False'.
--
-- When 'True', signal values are only set if they don't already exist in the
-- browser's store. See 'Hypermedia.Datastar.PatchSignals.psOnlyIfMissing'.
defaultOnlyIfMissing :: Bool
defaultOnlyIfMissing = False

-- | Default for the @autoRemove@ flag: 'True'.
--
-- When 'True', scripts executed via 'Hypermedia.Datastar.ExecuteScript.executeScript'
-- are automatically removed from the DOM after running.
defaultAutoRemove :: Bool
defaultAutoRemove = True

-- | Default element namespace: 'HtmlNs'.
defaultNamespace :: ElementNamespace
defaultNamespace = HtmlNs
