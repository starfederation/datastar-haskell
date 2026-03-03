-- |
-- Module      : Hypermedia.Datastar.PatchElements
-- Description : Send HTML fragments to patch the browser DOM
--
-- \"Patch elements\" is Datastar's primary mechanism for updating the page: the
-- server sends an HTML fragment and the browser morphs it into the DOM. The
-- morphing algorithm preserves focus, scroll position, and CSS transitions,
-- so partial page updates feel seamless.
--
-- The simplest case — send HTML and let Datastar match elements by @id@ —
-- needs only 'patchElements':
--
-- @
-- sendPatchElements gen (patchElements "\<div id=\\\"count\\\"\>42\<\/div\>")
-- @
--
-- To customise the patching behaviour, use record update syntax on the result
-- of 'patchElements':
--
-- @
-- sendPatchElements gen
--   (patchElements "\<li\>new item\<\/li\>")
--     { peSelector = Just \"#todo-list\"
--     , peMode = Append
--     }
-- @
--
-- To remove elements from the DOM, use 'removeElements' with a CSS selector:
--
-- @
-- sendPatchElements gen (removeElements \"#flash-message\")
-- @
module Hypermedia.Datastar.PatchElements where

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

-- | Configuration for a @datastar-patch-elements@ SSE event.
--
-- Construct values with 'patchElements' or 'removeElements', then customise
-- with record updates.
data PatchElements = PatchElements
  { peElements :: Maybe Text
  -- ^ The HTML fragment to patch into the DOM. 'Nothing' when removing
  -- elements (see 'removeElements').
  , peSelector :: Maybe Text
  -- ^ CSS selector for the target element. When 'Nothing' (the default),
  -- Datastar matches by the @id@ attribute of the root element in
  -- 'peElements'.
  , peMode :: ElementPatchMode
  -- ^ How to apply the patch. Default: 'Outer' (replace the matched
  -- element and its contents via morphing).
  , peUseViewTransition :: Bool
  -- ^ Whether to wrap the DOM update in a
  -- <https://developer.mozilla.org/en-US/docs/Web/API/View_Transition_API View Transition>.
  -- Default: 'False'.
  , peNamespace :: ElementNamespace
  -- ^ XML namespace for the patched elements. Default: 'HtmlNs'. Use
  -- 'SvgNs' or 'MathmlNs' when patching inline SVG or MathML.
  , peEventId :: Maybe Text
  -- ^ Optional SSE event ID. The browser uses this for reconnection —
  -- after a dropped connection it sends @Last-Event-ID@ so the server can
  -- resume from the right point.
  , peRetryDuration :: Int
  -- ^ SSE retry interval in milliseconds. Default: @1000@.
  }
  deriving (Eq, Show)

-- | Build a 'PatchElements' event with sensible defaults.
--
-- The HTML is sent as-is and Datastar matches target elements by their @id@
-- attribute.
--
-- @
-- patchElements "\<div id=\\\"greeting\\\"\>Hello!\<\/div\>"
-- @
patchElements :: Text -> PatchElements
patchElements html =
  PatchElements
    { peElements = if T.null html then Nothing else Just html
    , peSelector = Nothing
    , peMode = defaultPatchMode
    , peUseViewTransition = defaultUseViewTransition
    , peNamespace = defaultNamespace
    , peEventId = Nothing
    , peRetryDuration = defaultRetryDuration
    }

-- | Remove elements from the DOM matching a CSS selector.
--
-- @
-- removeElements \"#notification\"
-- removeElements \".stale-row\"
-- @
removeElements :: Text -> PatchElements
removeElements sel =
  PatchElements
    { peElements = Nothing
    , peSelector = Just sel
    , peMode = Remove
    , peUseViewTransition = defaultUseViewTransition
    , peNamespace = defaultNamespace
    , peEventId = Nothing
    , peRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: PatchElements -> DatastarEvent
toDatastarEvent pe =
  DatastarEvent
    { eventType = EventPatchElements
    , eventId = peEventId pe
    , retry = peRetryDuration pe
    , dataLines =
        concat
          [ maybe [] (\s -> ["selector " <> s]) (peSelector pe)
          , [ "mode " <> patchModeToText (peMode pe)
            | peMode pe /= defaultPatchMode
            ]
          , [ "useViewTransition true"
            | peUseViewTransition pe
            ]
          , [ "namespace " <> namespaceToText (peNamespace pe)
            | peNamespace pe /= defaultNamespace
            ]
          , maybe [] (map ("elements " <>) . T.lines) (peElements pe)
          ]
    }
