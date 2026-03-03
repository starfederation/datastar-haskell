module Hypermedia.Datastar.Logger
  ( DatastarLogger (..)
  , nullLogger
  , stderrLogger
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import System.IO (hPutStrLn, stderr)

data DatastarLogger = DatastarLogger
  { logDebug :: Text -> IO ()
  , logInfo :: Text -> IO ()
  , logWarn :: Text -> IO ()
  , logError :: Text -> IO ()
  }

nullLogger :: DatastarLogger
nullLogger =
  DatastarLogger
    { logDebug = const (pure ())
    , logInfo = const (pure ())
    , logWarn = const (pure ())
    , logError = const (pure ())
    }

stderrLogger :: DatastarLogger
stderrLogger =
  DatastarLogger
    { logDebug = logAt "DEBUG"
    , logInfo = logAt "INFO"
    , logWarn = logAt "WARN"
    , logError = logAt "ERROR"
    }
 where
  logAt :: Text -> Text -> IO ()
  logAt level msg = hPutStrLn stderr $ T.unpack ("[datastar] [" <> level <> "] " <> msg)
