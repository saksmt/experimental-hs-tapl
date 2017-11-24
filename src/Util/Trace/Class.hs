{-# LANGUAGE KindSignatures #-}

module Util.Trace.Class where

import Util.Trace.Log
import Util.Trace.Dumpable

import Prelude hiding (log)

class Monad m => MonadTrace (m :: * -> *) where
    nested :: m a -> m a
    log :: String -> LogLevel -> m ()

logTrace :: MonadTrace m => String -> m ()
logTrace = atTrace . log

logDebug :: MonadTrace m => String -> m ()
logDebug = atDebug . log

logInfo :: MonadTrace m => String -> m ()
logInfo = atInfo . log

logError :: MonadTrace m => String -> m ()
logError = atError . log

logCritical :: MonadTrace m => String -> m ()
logCritical = atCritical . log

operation :: MonadTrace m => String -> m ()
operation = logInfo

withArg :: (MonadTrace m, Dumpable a) => a -> m ()
withArg arg = do
    atInfo . log $ "Argument: "
    atInfo . dump $ arg
    return ()

dump :: (MonadTrace m, Dumpable a) => a -> LogLevel -> m a
dump value level = do
    sequence $ map ((flip log) level) $ dumpToStrings value
    return value
