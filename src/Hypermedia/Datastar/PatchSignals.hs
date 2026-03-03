-- |
-- Module      : Hypermedia.Datastar.PatchSignals
-- Description : Update the browser's reactive signal store
--
-- Signals are Datastar's reactive state — key-value pairs that live in the
-- browser and drive the UI. The server can update signals at any time by
-- sending a @datastar-patch-signals@ event.
--
-- Signal patching uses JSON Merge Patch semantics: set a key to update it,
-- set a key to @null@ to remove it, and omit a key to leave it unchanged.
--
-- @
-- sendPatchSignals gen (patchSignals \"{\\\"count\\\": 42}\")
-- @
--
-- To set initial state without overwriting values the user may have already
-- changed (e.g. form inputs), use 'psOnlyIfMissing':
--
-- @
-- sendPatchSignals gen
--   (patchSignals \"{\\\"name\\\": \\\"default\\\"}\")
--     { psOnlyIfMissing = True }
-- @
module Hypermedia.Datastar.PatchSignals where

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

-- | Configuration for a @datastar-patch-signals@ SSE event.
--
-- Construct values with 'patchSignals', then customise with record updates.
data PatchSignals = PatchSignals
  { psSignals :: Text
  -- ^ JSON object containing the signal values to patch. Uses JSON Merge
  -- Patch semantics: set a key to update it, set to @null@ to remove it.
  , psOnlyIfMissing :: Bool
  -- ^ When 'True', signal values are only set if the key doesn't already
  -- exist in the browser's store. Useful for setting initial state that
  -- shouldn't overwrite user changes (e.g. form input defaults).
  -- Default: 'False'.
  , psEventId :: Maybe Text
  -- ^ Optional SSE event ID for reconnection. See 'Hypermedia.Datastar.PatchElements.PatchElements' for details.
  , psRetryDuration :: Int
  -- ^ SSE retry interval in milliseconds. Default: @1000@.
  }
  deriving (Eq, Show)

-- | Build a 'PatchSignals' event with sensible defaults.
--
-- The argument is a JSON object (as 'Text') describing the signals to update.
--
-- @
-- patchSignals \"{\\\"count\\\": 42, \\\"label\\\": \\\"hello\\\"}\"
-- @
patchSignals :: Text -> PatchSignals
patchSignals sigs =
  PatchSignals
    { psSignals = sigs
    , psOnlyIfMissing = defaultOnlyIfMissing
    , psEventId = Nothing
    , psRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: PatchSignals -> DatastarEvent
toDatastarEvent ps =
  DatastarEvent
    { eventType = EventPatchSignals
    , eventId = psEventId ps
    , retry = psRetryDuration ps
    , dataLines = ifMissingLine ++ signalLines
    }
 where
  ifMissingLine = ["onlyIfMissing true" | psOnlyIfMissing ps]
  signalLines = map ("signals " <>) $ T.lines $ psSignals ps
