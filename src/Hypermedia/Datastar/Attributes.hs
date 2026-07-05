{- | Smart constructors and modifier combinators for Datastar HTML attributes.

Every Datastar attribute is a @(name, value)@ pair — the value of the
'Attribute' type alias. Pair it with whatever HTML library you use:

> -- lucid2:
> button_ [ uncurry makeAttributes (onClick "$count++") ] "Inc"

Modifiers (the @__suffix@ parts of an attribute name) are plain
@Attribute -> Attribute@ functions you compose with '(.)' or '(&)':

> import Data.Function ((&))
> onInput "@get('/search')"
>   & debounce 200
>   & prevent
> -- => ("data-on:input__debounce.200ms__prevent", "@get('/search')")

For an event the library doesn't have a shortcut for, 'dataOn' takes any
'Text':

> dataOn "my-custom-event" "..."
-}
module Hypermedia.Datastar.Attributes
  ( -- * The attribute pair
    Attribute

    -- * Event handlers
  , dataOn

    -- ** Common event shortcuts
  , onClick
  , onSubmit
  , onInput
  , onChange
  , onFocus
  , onBlur
  , onKeyDown
  , onKeyUp
  , onKeyPress
  , onMouseDown
  , onMouseUp
  , onMouseMove
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  , onWheel
  , onScroll
  , onResize
  , onContextMenu
  , onTouchStart
  , onTouchEnd
  , onTouchMove
  , onDragStart
  , onDragEnd
  , onDrop
  , onCopy
  , onCut
  , onPaste
  , onLoad
  , onAnimationStart
  , onAnimationEnd
  , onTransitionEnd

    -- * Reactive attributes
  , dataAttr
  , dataAttrs
  , dataClass
  , dataClasses
  , dataStyle
  , dataStyles
  , dataText
  , dataShow

    -- * Signals
  , dataSignals
  , dataSignalsJson
  , dataComputed
  , dataBind
  , dataBindValue
  , dataJsonSignals

    -- * Lifecycle / refs
  , dataInit
  , dataEffect
  , dataRef
  , dataRefValue
  , dataIndicator
  , dataIndicatorValue

    -- * Lifecycle events
  , dataOnIntersect
  , dataOnInterval
  , dataOnSignalPatch
  , dataOnSignalPatchFilter

    -- * Morph / ignore controls
  , dataIgnore
  , dataIgnoreMorph
  , dataPreserveAttr

    -- * Modifier combinators

    -- ** Event flags
  , prevent
  , stop
  , once
  , capture
  , passive
  , window
  , document_
  , outside
  , viewTransition

    -- ** Timing
  , debounce
  , debounceWith
  , throttle
  , throttleWith
  , delay

    -- ** Casing
  , camelCase
  , kebabCase
  , snakeCase
  , pascalCase

    -- ** on-intersect
  , full
  , half
  , threshold
  , exit

    -- ** on-interval
  , duration
  , durationWith

    -- ** bind
  , bindProp
  , bindEvents

    -- ** signals \/ json-signals \/ ignore
  , ifMissing
  , terse
  , self

    -- * Modifier tag constants
    -- $tags
  , leading
  , noTrailing
  , noLeading
  , trailing

    -- * Escape hatches
  , withModifier
  , withModifierTags
  , rawAttr
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | A @(name, value)@ pair ready to feed into any HTML library.
type Attribute = (Text, Text)

-- ---------------------------------------------------------------------------
-- Event handlers
-- ---------------------------------------------------------------------------

{- | @data-on:\<event\>="\<expr\>"@. Use the shortcuts below for common events,
or call directly for custom events (e.g. @dataOn \"my-custom-event\" ...@).
-}
dataOn :: Text -> Text -> Attribute
dataOn ev expr = ("data-on:" <> ev, expr)

onClick, onSubmit, onInput, onChange, onFocus, onBlur :: Text -> Attribute
onClick = dataOn "click"
onSubmit = dataOn "submit"
onInput = dataOn "input"
onChange = dataOn "change"
onFocus = dataOn "focus"
onBlur = dataOn "blur"

onKeyDown, onKeyUp, onKeyPress :: Text -> Attribute
onKeyDown = dataOn "keydown"
onKeyUp = dataOn "keyup"
onKeyPress = dataOn "keypress"

onMouseDown
  , onMouseUp
  , onMouseMove
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
    :: Text -> Attribute
onMouseDown = dataOn "mousedown"
onMouseUp = dataOn "mouseup"
onMouseMove = dataOn "mousemove"
onMouseEnter = dataOn "mouseenter"
onMouseLeave = dataOn "mouseleave"
onMouseOver = dataOn "mouseover"
onMouseOut = dataOn "mouseout"

onWheel, onScroll, onResize, onContextMenu :: Text -> Attribute
onWheel = dataOn "wheel"
onScroll = dataOn "scroll"
onResize = dataOn "resize"
onContextMenu = dataOn "contextmenu"

onTouchStart, onTouchEnd, onTouchMove :: Text -> Attribute
onTouchStart = dataOn "touchstart"
onTouchEnd = dataOn "touchend"
onTouchMove = dataOn "touchmove"

onDragStart, onDragEnd, onDrop :: Text -> Attribute
onDragStart = dataOn "dragstart"
onDragEnd = dataOn "dragend"
onDrop = dataOn "drop"

onCopy, onCut, onPaste :: Text -> Attribute
onCopy = dataOn "copy"
onCut = dataOn "cut"
onPaste = dataOn "paste"

onLoad :: Text -> Attribute
onLoad = dataOn "load"

onAnimationStart, onAnimationEnd, onTransitionEnd :: Text -> Attribute
onAnimationStart = dataOn "animationstart"
onAnimationEnd = dataOn "animationend"
onTransitionEnd = dataOn "transitionend"

-- ---------------------------------------------------------------------------
-- Reactive attributes
-- ---------------------------------------------------------------------------

-- | @data-attr:\<name\>="\<expr\>"@
dataAttr :: Text -> Text -> Attribute
dataAttr name expr = ("data-attr:" <> name, expr)

-- | @data-attr="\<jsObject\>"@ — set multiple attributes at once.
dataAttrs :: Text -> Attribute
dataAttrs jsObj = ("data-attr", jsObj)

-- | @data-class:\<name\>="\<expr\>"@
dataClass :: Text -> Text -> Attribute
dataClass name expr = ("data-class:" <> name, expr)

-- | @data-class="\<jsObject\>"@ — toggle several classes from one expression.
dataClasses :: Text -> Attribute
dataClasses jsObj = ("data-class", jsObj)

-- | @data-style:\<property\>="\<expr\>"@
dataStyle :: Text -> Text -> Attribute
dataStyle prop expr = ("data-style:" <> prop, expr)

-- | @data-style="\<jsObject\>"@ — set several style properties at once.
dataStyles :: Text -> Attribute
dataStyles jsObj = ("data-style", jsObj)

-- | @data-text="\<expr\>"@
dataText :: Text -> Attribute
dataText expr = ("data-text", expr)

-- | @data-show="\<expr\>"@
dataShow :: Text -> Attribute
dataShow expr = ("data-show", expr)

-- ---------------------------------------------------------------------------
-- Signals
-- ---------------------------------------------------------------------------

-- | @data-signals:\<name\>="\<expr\>"@
dataSignals :: Text -> Text -> Attribute
dataSignals name expr = ("data-signals:" <> name, expr)

-- | @data-signals="\<jsObject\>"@
dataSignalsJson :: Text -> Attribute
dataSignalsJson jsObj = ("data-signals", jsObj)

-- | @data-computed:\<name\>="\<expr\>"@
dataComputed :: Text -> Text -> Attribute
dataComputed name expr = ("data-computed:" <> name, expr)

-- | @data-bind:\<signal\>@
dataBind :: Text -> Attribute
dataBind signal = ("data-bind:" <> signal, "")

-- | @data-bind="\<signal\>"@
dataBindValue :: Text -> Attribute
dataBindValue signal = ("data-bind", signal)

-- | @data-json-signals@. Wrap with 'terse' for compact output.
dataJsonSignals :: Attribute
dataJsonSignals = ("data-json-signals", "")

-- ---------------------------------------------------------------------------
-- Lifecycle / refs
-- ---------------------------------------------------------------------------

-- | @data-init="\<expr\>"@
dataInit :: Text -> Attribute
dataInit expr = ("data-init", expr)

-- | @data-effect="\<expr\>"@
dataEffect :: Text -> Attribute
dataEffect expr = ("data-effect", expr)

-- | @data-ref:\<name\>@
dataRef :: Text -> Attribute
dataRef name = ("data-ref:" <> name, "")

-- | @data-ref="\<name\>"@
dataRefValue :: Text -> Attribute
dataRefValue name = ("data-ref", name)

-- | @data-indicator:\<signal\>@
dataIndicator :: Text -> Attribute
dataIndicator signal = ("data-indicator:" <> signal, "")

-- | @data-indicator="\<signal\>"@
dataIndicatorValue :: Text -> Attribute
dataIndicatorValue signal = ("data-indicator", signal)

-- ---------------------------------------------------------------------------
-- Lifecycle events
-- ---------------------------------------------------------------------------

{- | @data-on-intersect="\<expr\>"@. Combine with 'full', 'half', 'threshold',
'once', 'exit'.
-}
dataOnIntersect :: Text -> Attribute
dataOnIntersect expr = ("data-on-intersect", expr)

-- | @data-on-interval="\<expr\>"@. Combine with 'duration'.
dataOnInterval :: Text -> Attribute
dataOnInterval expr = ("data-on-interval", expr)

-- | @data-on-signal-patch="\<expr\>"@. Pair with 'dataOnSignalPatchFilter'.
dataOnSignalPatch :: Text -> Attribute
dataOnSignalPatch expr = ("data-on-signal-patch", expr)

-- | @data-on-signal-patch-filter="\<jsObject\>"@
dataOnSignalPatchFilter :: Text -> Attribute
dataOnSignalPatchFilter v = ("data-on-signal-patch-filter", v)

-- ---------------------------------------------------------------------------
-- Morph / ignore controls
-- ---------------------------------------------------------------------------

-- | @data-ignore@. Wrap with 'self' for the self-only variant.
dataIgnore :: Attribute
dataIgnore = ("data-ignore", "")

-- | @data-ignore-morph@
dataIgnoreMorph :: Attribute
dataIgnoreMorph = ("data-ignore-morph", "")

-- | @data-preserve-attr="\<comma-list\>"@
dataPreserveAttr :: Text -> Attribute
dataPreserveAttr names = ("data-preserve-attr", names)

-- ---------------------------------------------------------------------------
-- Modifier combinators
-- ---------------------------------------------------------------------------

-- | Append @__\<label\>@ to the attribute name.
withModifier :: Text -> Attribute -> Attribute
withModifier label (k, v) = (k <> "__" <> label, v)

-- | Append @__\<label\>.\<tag\>.\<tag\>...@ to the attribute name.
withModifierTags :: Text -> [Text] -> Attribute -> Attribute
withModifierTags label tags (k, v) =
  (k <> "__" <> label <> T.concat (map ("." <>) tags), v)

{- | Build any @(name, value)@ pair directly. Escape hatch for attributes or
modifiers this module doesn't model.
-}
rawAttr :: Text -> Text -> Attribute
rawAttr = (,)

-- Event flags --------------------------------------------------------------

prevent, stop, once, capture, passive :: Attribute -> Attribute
prevent = withModifier "prevent"
stop = withModifier "stop"
once = withModifier "once"
capture = withModifier "capture"
passive = withModifier "passive"

-- | Attach the listener to @window@ instead of the element.
window :: Attribute -> Attribute
window = withModifier "window"

-- | Attach the listener to @document@ instead of the element.
document_ :: Attribute -> Attribute
document_ = withModifier "document"

-- | Fire only when the event target is outside the element.
outside :: Attribute -> Attribute
outside = withModifier "outside"

-- | Wrap the handler in a View Transition.
viewTransition :: Attribute -> Attribute
viewTransition = withModifier "viewtransition"

-- Timing -------------------------------------------------------------------

-- | @__debounce.\<ms\>ms@
debounce :: Int -> Attribute -> Attribute
debounce ms = withModifierTags "debounce" [renderMs ms]

-- | @__debounce.\<ms\>ms.\<tag\>...@ — pass 'leading' and\/or 'noTrailing'.
debounceWith :: Int -> [Text] -> Attribute -> Attribute
debounceWith ms tags = withModifierTags "debounce" (renderMs ms : tags)

-- | @__throttle.\<ms\>ms@
throttle :: Int -> Attribute -> Attribute
throttle ms = withModifierTags "throttle" [renderMs ms]

-- | @__throttle.\<ms\>ms.\<tag\>...@ — pass 'noLeading' and\/or 'trailing'.
throttleWith :: Int -> [Text] -> Attribute -> Attribute
throttleWith ms tags = withModifierTags "throttle" (renderMs ms : tags)

-- | @__delay.\<ms\>ms@
delay :: Int -> Attribute -> Attribute
delay ms = withModifierTags "delay" [renderMs ms]

-- Casing -------------------------------------------------------------------

camelCase, kebabCase, snakeCase, pascalCase :: Attribute -> Attribute
camelCase = withModifierTags "case" ["camel"]
kebabCase = withModifierTags "case" ["kebab"]
snakeCase = withModifierTags "case" ["snake"]
pascalCase = withModifierTags "case" ["pascal"]

-- on-intersect -------------------------------------------------------------

-- | Trigger only when 100% of the element is visible.
full :: Attribute -> Attribute
full = withModifier "full"

-- | Trigger when 50% of the element is visible.
half :: Attribute -> Attribute
half = withModifier "half"

-- | @__threshold.\<percent\>@ (0..100, clamped at runtime).
threshold :: Int -> Attribute -> Attribute
threshold pct = withModifierTags "threshold" [T.pack (show pct)]

-- | Trigger on exit rather than entry.
exit :: Attribute -> Attribute
exit = withModifier "exit"

-- on-interval --------------------------------------------------------------

-- | @__duration.\<ms\>ms@
duration :: Int -> Attribute -> Attribute
duration ms = withModifierTags "duration" [renderMs ms]

-- | @__duration.\<ms\>ms.\<tag\>...@ — pass 'leading' to fire immediately.
durationWith :: Int -> [Text] -> Attribute -> Attribute
durationWith ms tags = withModifierTags "duration" (renderMs ms : tags)

-- bind ---------------------------------------------------------------------

-- | @__prop.\<name\>@ — bind to an element property instead of an attribute.
bindProp :: Text -> Attribute -> Attribute
bindProp name = withModifierTags "prop" [name]

-- | @__event.\<ev\>.\<ev\>...@ — which DOM events trigger sync.
bindEvents :: [Text] -> Attribute -> Attribute
bindEvents events = withModifierTags "event" events

-- signals / json-signals / ignore -----------------------------------------

-- | @__ifmissing@ — only set the signal if it doesn't already exist.
ifMissing :: Attribute -> Attribute
ifMissing = withModifier "ifmissing"

-- | @__terse@ — emit JSON without indentation.
terse :: Attribute -> Attribute
terse = withModifier "terse"

-- | @__self@ — restrict @data-ignore@ to the element itself, not descendants.
self :: Attribute -> Attribute
self = withModifier "self"

-- ---------------------------------------------------------------------------
-- Tag constants
-- ---------------------------------------------------------------------------

{- $tags
Plain 'Text' values for use with the @*With@ combinators.
-}

leading, noTrailing, noLeading, trailing :: Text
leading = "leading"
noTrailing = "notrailing"
noLeading = "noleading"
trailing = "trailing"

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

renderMs :: Int -> Text
renderMs n = T.pack (show n) <> "ms"
