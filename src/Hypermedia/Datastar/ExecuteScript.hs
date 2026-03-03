-- |
-- Module      : Hypermedia.Datastar.ExecuteScript
-- Description : Execute JavaScript in the browser via SSE
--
-- Sometimes you need a one-shot browser-side effect that doesn't fit neatly
-- into DOM patching or signal updates — redirecting to another page, focusing
-- an input, triggering a download, or calling a browser API.
-- 'executeScript' lets the server push arbitrary JavaScript to the browser.
--
-- Under the hood, Datastar appends a @\<script\>@ tag to @\<body\>@. By default
-- the script tag removes itself from the DOM after executing (see 'esAutoRemove').
--
-- Note: per the Datastar protocol, script execution uses the
-- @datastar-patch-elements@ event type — there is no separate event type for
-- scripts.
--
-- @
-- sendExecuteScript gen (executeScript \"window.location = \\\"/dashboard\\\"\")
-- @
--
-- To add attributes to the generated @\<script\>@ tag:
--
-- @
-- sendExecuteScript gen
--   (executeScript \"import(\\\"/modules\/chart.js\\\").then(m => m.render())\")
--     { esAttributes = [\"type=\\\"module\\\"\"] }
-- @
module Hypermedia.Datastar.ExecuteScript where

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

-- | Configuration for executing a script in the browser.
--
-- Construct values with 'executeScript', then customise with record updates.
data ExecuteScript = ExecuteScript
  { esScript :: Text
  -- ^ The JavaScript code to execute.
  , esAutoRemove :: Bool
  -- ^ Whether the @\<script\>@ tag should remove itself from the DOM after
  -- executing. Default: 'True'. Set to 'False' if the script defines
  -- functions or variables that need to persist in the page.
  , esAttributes :: [Text]
  -- ^ Extra attributes to add to the @\<script\>@ tag. For example,
  -- @[\"type=\\\"module\\\"\"]@ to use ES module imports, or
  -- @[\"nonce=\\\"abc123\\\"\"]@ for CSP compliance.
  , esEventId :: Maybe Text
  -- ^ Optional SSE event ID for reconnection.
  , esRetryDuration :: Int
  -- ^ SSE retry interval in milliseconds. Default: @1000@.
  }
  deriving (Eq, Show)

-- | Build an 'ExecuteScript' event with sensible defaults.
--
-- The argument is the JavaScript source code to run in the browser.
--
-- @
-- executeScript \"document.getElementById(\\\"name\\\").focus()\"
-- @
executeScript :: Text -> ExecuteScript
executeScript js =
  ExecuteScript
    { esScript = js
    , esAutoRemove = defaultAutoRemove
    , esAttributes = []
    , esEventId = Nothing
    , esRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: ExecuteScript -> DatastarEvent
toDatastarEvent es =
  DatastarEvent
    { eventType = EventPatchElements -- Correct, there is no EventExecuteScript, see the ADR
    , eventId = esEventId es
    , retry = esRetryDuration es
    , dataLines =
        [ "selector body"
        , "mode append"
        ]
          <> buildScriptLines es
    }

buildScriptLines :: ExecuteScript -> [Text]
buildScriptLines es =
  case T.lines (esScript es) of
    [] -> ["elements " <> openTag <> closeTag]
    [single] -> ["elements " <> openTag <> single <> closeTag]
    multiple ->
      ["elements " <> openTag]
        <> map ("elements " <>) multiple
        <> ["elements " <> closeTag]
 where
  openTag :: Text
  openTag =
    "<script"
      <> (if esAutoRemove es then " data-effect=\"el.remove()\"" else "")
      <> foldMap (" " <>) (esAttributes es)
      <> ">"

  closeTag :: Text
  closeTag = "</script>"
