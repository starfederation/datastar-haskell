{- |
Module      : Hypermedia.Datastar.WAI
Description : SSE streaming and request handling for WAI

This module connects Datastar to WAI (Web Application Interface), Haskell's
standard HTTP server interface. It provides:

* 'sseResponse' — create a streaming SSE response with a
  'ServerSentEventGenerator' callback
* 'sendPatchElements', 'sendPatchSignals', 'sendExecuteScript' — send
  Datastar events through the open connection
* 'readSignals' — decode signals sent by the browser (from query params on
  GET, or from the request body on POST)
* 'isDatastarRequest' — distinguish Datastar SSE requests from normal
  page loads

=== Streaming architecture

'sseResponse' uses WAI's @responseStream@ to hold the HTTP connection open.
You receive a 'ServerSentEventGenerator' and call @send*@ functions as many
times as needed. The connection stays open until your callback returns (or
the client disconnects).

=== Thread safety

The 'ServerSentEventGenerator' uses an internal 'Control.Concurrent.MVar.MVar'
lock, so it is safe to call @send*@ functions from multiple threads
concurrently.

=== How signals flow from browser to server

When the browser makes a Datastar request, signals are sent as JSON:

* __GET requests__: signals are URL-encoded in the @datastar@ query parameter
* __POST requests__: signals are in the request body as JSON

Use 'readSignals' with any 'Data.Aeson.FromJSON' instance to decode them.
-}
module Hypermedia.Datastar.WAI where

import Control.Concurrent.MVar
import Control.Exception

import Data.Text (Text)
import Data.Text.Encoding qualified as TE

import Data.Aeson (FromJSON)
import Data.Aeson qualified as A

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS8

import Data.List (find)
import Data.Maybe (listToMaybe)

import Network.HTTP.Types qualified as WAI
import Network.Wai qualified as WAI

import Hypermedia.Datastar.Types

import Hypermedia.Datastar.ExecuteScript qualified as ES
import Hypermedia.Datastar.Logger qualified as Logger
import Hypermedia.Datastar.PatchElements qualified as PE
import Hypermedia.Datastar.PatchSignals qualified as PS

data Compressor = Compressor
  { compressorEncoding :: BS.ByteString
  -- ^ The @Content-Encoding@ token, e.g. @"br"@ for Brotli compression.
  , compressorWrap
      :: (BSB.Builder -> IO ())
      -> IO ()
      -> IO (BSB.Builder -> IO (), IO (), IO ())
  {- ^ @compressorWrap rawWrite rawFlush@ returns @(write, flush, finish)@.

  * @write@ compresses and forwards bytes for an event;
  * @flush@ flushes the compression encoded and then the underlying raw transport;
  * @finish@ closes the compression stream.
  -}
  }

data CompressionStrategy
  = ServerPriority
  | ClientPriority
  | Forced
  deriving (Eq, Show)

{- | Faithful port of the Go reference parser: split on ',', trim surrounding
whitespace, take the token before ';', drop empties. No q-value handling,
no case folding, no wildcard logic.
-}
parseEncodings :: BS.ByteString -> [BS.ByteString]
parseEncodings header =
  [ token
  | part <- BS8.split ',' header
  , let token = BS8.takeWhile (/= ';') (strip part)
  , not (BS8.null token)
  ]
 where
  strip = BS8.dropWhile isOWS . BS8.dropWhileEnd isOWS
  isOWS c = c == ' ' || c == '\t'

{- | The encoding tokens of a request's @Accept-Encoding@ header, in order.
An absent or empty header yields [].
-}
clientEncodings :: WAI.Request -> [BS.ByteString]
clientEncodings req =
  maybe
    []
    parseEncodings
    (lookup WAI.hAcceptEncoding $ WAI.requestHeaders req)

{- | Pick a compressor for a request's @Accept-Encoding@ header using the given
'CompressionStrategy', or 'Nothing' if none applies. Token matching is exact
(case-sensitive), mirroring the Go reference.
-}
negotiateWith :: CompressionStrategy -> [Compressor] -> WAI.Request -> Maybe Compressor
negotiateWith strategy compressors req =
  case strategy of
    ServerPriority ->
      -- first server compressor whose token the client sent
      find (\c -> compressorEncoding c `elem` accepted) compressors
    ClientPriority ->
      -- first client token (header order) that the server offers
      listToMaybe
        [ c
        | enc <- accepted
        , c <- compressors
        , compressorEncoding c == enc
        ]
    Forced ->
      -- first configured compressor, regardless of Accept-Encoding
      listToMaybe compressors
 where
  accepted = clientEncodings req

{- | An opaque handle for sending SSE events to the browser.

Obtain one from the callback passed to 'sseResponse'. The handle is
thread-safe — you can send events from multiple threads concurrently.

You don't construct these directly; 'sseResponse' creates one for you.
-}
data ServerSentEventGenerator = ServerSentEventGenerator
  { sseWrite :: BSB.Builder -> IO ()
  , sseFlush :: IO ()
  , sseLock :: MVar ()
  , sseLogger :: Logger.DatastarLogger
  }

{- | Create a WAI 'WAI.Response' that streams SSE events.

The callback receives a 'ServerSentEventGenerator' for sending events.
The SSE connection stays open until the callback returns.

@
app :: WAI.Request -> (WAI.Response -> IO b) -> IO b
app req respond =
  respond $ sseResponse $ \\gen -> do
    sendPatchElements gen (patchElements "\<div id=\\\"msg\\\"\>Hello\<\/div\>")
@
-}
sseResponse :: Logger.DatastarLogger -> (ServerSentEventGenerator -> IO ()) -> WAI.Response
sseResponse logger = sseResponse' logger Nothing

sseResponseWith
  :: Logger.DatastarLogger
  -> [Compressor]
  -> WAI.Request
  -> (ServerSentEventGenerator -> IO ())
  -> WAI.Response
sseResponseWith = sseResponseWithStrategy ServerPriority

sseResponseWithStrategy
  :: CompressionStrategy
  -> Logger.DatastarLogger
  -> [Compressor]
  -> WAI.Request
  -> (ServerSentEventGenerator -> IO ())
  -> WAI.Response
sseResponseWithStrategy strategy logger compressors req =
  sseResponse' logger (negotiateWith strategy compressors req)

sseResponse'
  :: Logger.DatastarLogger
  -> Maybe Compressor
  -> (ServerSentEventGenerator -> IO ())
  -> WAI.Response
sseResponse' logger chosen callback =
  WAI.responseStream
    WAI.status200
    headers
    action
 where
  headers =
    [ ("Cache-Control", "no-cache")
    , ("Content-Type", "text/event-stream")
    , ("Connection", "keep-alive")
    ]
      <> case chosen of
        Just c -> [("Content-Encoding", compressorEncoding c)]
        Nothing -> []

  action rawWrite rawFlush = do
    (write, flush, finish) <- case chosen of
      Nothing -> pure (rawWrite, rawFlush, pure ())
      Just c -> compressorWrap c rawWrite rawFlush
    lock <- newMVar ()
    let gen =
          ServerSentEventGenerator
            { sseWrite = write
            , sseFlush = flush
            , sseLock = lock
            , sseLogger = logger
            }
    callback gen `finally` ignoreIOErrors finish

ignoreIOErrors :: IO () -> IO ()
ignoreIOErrors act = act `catch` handler
 where
  handler :: IOException -> IO ()
  handler _ = pure ()

send :: ServerSentEventGenerator -> DatastarEvent -> IO ()
send gen event = do
  let rendered = renderEvent event

  bracket_
    (takeMVar $ sseLock gen)
    (putMVar (sseLock gen) ())
    $ do
      sseWrite gen rendered
      sseFlush gen

-- | Send a 'PE.PatchElements' event, morphing HTML into the browser's DOM.
sendPatchElements :: ServerSentEventGenerator -> PE.PatchElements -> IO ()
sendPatchElements gen pe = send gen $ PE.toDatastarEvent pe

-- | Send a 'PS.PatchSignals' event, updating the browser's reactive signal store.
sendPatchSignals :: ServerSentEventGenerator -> PS.PatchSignals -> IO ()
sendPatchSignals gen ps = send gen $ PS.toDatastarEvent ps

-- | Send an 'ES.ExecuteScript' event, running JavaScript in the browser.
sendExecuteScript :: ServerSentEventGenerator -> ES.ExecuteScript -> IO ()
sendExecuteScript gen es = send gen $ ES.toDatastarEvent es

{- | Decode signals sent by the browser in a Datastar request.

For GET requests, signals are URL-encoded in the @datastar@ query parameter.
For POST requests (and other methods), signals are read from the request
body as JSON.

Define a Haskell data type with a 'FromJSON' instance to decode into:

@
data MySignals = MySignals { count :: Int, label :: Text }
  deriving (Generic)
  deriving anyclass (FromJSON)

handler :: WAI.Request -> IO ()
handler req = do
  Right signals <- readSignals req
  putStrLn $ \"Count is: \" <> show (count signals)
@
-}
readSignals :: (FromJSON a) => WAI.Request -> IO (Either String a)
readSignals req
  | WAI.requestMethod req == "GET"
      || WAI.requestMethod req == "DELETE" =
      pure $ parseFromQuery req
  | otherwise =
      parseFromBody req

parseFromQuery :: (FromJSON a) => WAI.Request -> Either String a
parseFromQuery req =
  case lookup "datastar" (WAI.queryString req) of
    (Just (Just val)) ->
      A.eitherDecodeStrict $ WAI.urlDecode True val
    _ -> Left "missing 'datastar' query parameter"

parseFromBody :: (FromJSON a) => WAI.Request -> IO (Either String a)
parseFromBody req = A.eitherDecode <$> WAI.strictRequestBody req

{- | Check whether a request was initiated by Datastar.

Datastar adds a @datastar-request@ header to its SSE requests. Use this to
distinguish Datastar requests from normal page loads — for example, to
serve either an SSE stream or a full HTML page from the same route.

@
app req respond
  | isDatastarRequest req =
      respond $ sseResponse $ \\gen -> ...
  | otherwise =
      respond $ responseLBS status200 [] fullPageHtml
@
-}
isDatastarRequest :: WAI.Request -> Bool
isDatastarRequest req = any ((== "datastar-request") . fst) (WAI.requestHeaders req)

renderEvent :: DatastarEvent -> BSB.Builder
renderEvent event =
  mconcat
    [ BSB.stringUtf8 "event: " <> text (eventTypeToText (eventType event)) <> newline
    , maybe mempty (\eid -> BSB.stringUtf8 "id: " <> text eid <> newline) (eventId event)
    , if retry event /= defaultRetryDuration
        then BSB.stringUtf8 "retry: " <> BSB.intDec (retry event) <> newline
        else mempty
    , foldMap (\line -> BSB.stringUtf8 "data: " <> text line <> newline) (dataLines event)
    , newline
    ]
 where
  text :: Text -> BSB.Builder
  text = TE.encodeUtf8Builder

  newline :: BSB.Builder
  newline = BSB.charUtf8 '\n'
