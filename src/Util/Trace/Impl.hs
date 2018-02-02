{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module Util.Trace.Impl (TraceM, TraceLog, runTraceM, execTraceM, evalTraceM) where

import Prelude hiding (log)

import Data.Semigroup ((<>), Semigroup)
import Util.Trace.Class
import Util.Trace.Log
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

data LogTree a = LogEntry a | LoggedLevel [LogTree a] | EmptyLog deriving (Foldable, Traversable, Functor, Show)
data LoggedMessage = LoggedMessage { messageLogLevel :: LogLevel, logMessage :: String } deriving (Show)
type Logged = LogTree LoggedMessage

instance Monoid (LogTree a) where
    mempty = EmptyLog
    EmptyLog `mappend` x = x
    x `mappend` EmptyLog = x
    x@(LogEntry _) `mappend` (LoggedLevel xs) = LoggedLevel $ x:xs
    (LoggedLevel xs) `mappend` x@(LogEntry _) = LoggedLevel $ xs ++ [x]
    x@(LogEntry _) `mappend` y@(LogEntry _) = LoggedLevel [x, y]

instance Monad LogTree where
    return x = LogEntry x
    (EmptyLog) >>= f =  EmptyLog
    (LogEntry x) >>= f = f x
    (LoggedLevel xs) >>= f = LoggedLevel $ map (>>= f) xs

instance Applicative LogTree

filter_ :: (a -> Bool) -> LogTree a -> LogTree a
filter_ pred log = filterElem =<< log
    where filterElem elem = if pred elem then LogEntry $ elem else EmptyLog

foldLog :: Monoid b => (Int -> a -> b) -> LogTree a -> b
foldLog foldF = go 0
    where go currentLevel EmptyLog = mempty
          go currentLevel (LogEntry a) = foldF currentLevel a
          go currentLevel (LoggedLevel xs) = mconcat $ fmap (go $ currentLevel + 1) xs

logToString :: Int -> Char -> Maybe Int -> Int -> LoggedMessage -> [String]
logToString padSize padSymbol maxLevel level entry =
    let actualLevel = fromMaybe level $ min <$> maxLevel <*> pure level
        totalPadSize = padSize * actualLevel
        pad = take totalPadSize $ repeat padSymbol
        messages = lines $ logMessage entry
    in map (pad++) messages

instance Log TraceLog where
    toLogLines (LoggerSettings
        { logPaddingSymbol = symbol
        , logPaddingSize = size
        , logNestingMaximumLevel = max
        , logLevel = level }) traceLog =
            let log = LoggedLevel $ getTraceLog traceLog
                needsToBeDisplayed entry = messageLogLevel entry >= level
                visibleLog = filter_ needsToBeDisplayed log
                toLines = logToString size symbol max
                logLines = foldLog toLines visibleLog
            in logLines

newtype TraceLog = TraceLog { getTraceLog :: [Logged] } deriving (Semigroup, Monoid)
newtype TraceM v = TraceM { runTraceM :: (v, TraceLog) } deriving (Functor)

execTraceM = snd . runTraceM
evalTraceM = fst . runTraceM

instance Applicative TraceM where
    pure v = TraceM (v, mempty)
    packedF <*> m = let (oldV, loggedSoFar) = runTraceM m
                        (f, logFromFunctionGeneration) = runTraceM packedF
                    in TraceM (f oldV, loggedSoFar <> logFromFunctionGeneration)

instance Monad TraceM where
    return v = TraceM (v, mempty)
    m >>= f = let (oldV, loggedSoFar) = runTraceM m
                  (newV, newLog) = runTraceM $ f oldV
              in TraceM (newV, loggedSoFar <> newLog)

instance MonadTrace TraceM where
    nested block = let (v, oldLog) = runTraceM block in TraceM (v, TraceLog [LoggedLevel $ getTraceLog oldLog])
    log message level = TraceM((), TraceLog [LogEntry $ LoggedMessage level message])
