{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module Util.Trace.Impl (TraceM, TraceLog, runTraceM) where

import Prelude hiding (log)

import Data.Semigroup ((<>), Semigroup)
import Util.Trace.Class
import Util.Trace.Log

data LogTree a = LogEntry a | LoggedLevel [LogTree a] deriving (Foldable, Traversable, Functor, Show)
data LoggedMessage = LoggedMessage LogLevel String deriving (Show)
type Logged = LogTree LoggedMessage

instance Log TraceLog where
    printLog (LoggerSettings
        { logPaddingSymbol = symbol
        , logPaddingSize = size
        , logNestingMaximumLevel = max
        , logLevel = level }) log = do
        -- FILTER -> CONVERT TO LINES -> INDENT -> GLUE UP
--             putStrLn $ intercalate "\n" $ indent symbol size max $ fmap extractMessage $ filterLLL level log
            return ()

newtype TraceLog = TraceLog { getTraceLog :: [Logged] } deriving (Semigroup, Monoid)
newtype TraceM v = TraceM { runTraceM :: (v, TraceLog) } deriving (Functor)
{-
instance Functor TraceM where
    f `fmap` m = let (oldV, loggedSoFar) = runTraceM m
                     newV = f oldV
                 in TraceM (newV, loggedSoFar)-}

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
