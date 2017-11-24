module Util.Trace.Log where

data LogLevel = LogLevelTrace
              | LogLevelDebug
              | LogLevelInfo
              | LogLevelWarn
              | LogLevelError
              | LogLevelCritical
              deriving (Show, Eq, Ord, Enum)

atTrace f = f LogLevelTrace
atDebug f = f LogLevelDebug
atInfo f = f LogLevelInfo
atWarn f = f LogLevelWarn
atError f = f LogLevelError
atCritical f = f LogLevelCritical

data LoggerSettings = LoggerSettings
    { logLevel :: LogLevel
    , logPaddingSymbol :: Char
    , logPaddingSize :: Int
    , logNestingMaximumLevel :: Maybe Int
    }

loggerSettings = LoggerSettings
    { logLevel = LogLevelInfo
    , logPaddingSymbol = ' '
    , logPaddingSize = 2
    , logNestingMaximumLevel = Nothing
    }

class Log l where
    printLog :: LoggerSettings -> l -> IO ()
