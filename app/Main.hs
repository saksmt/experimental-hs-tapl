module Main where

import Options.Applicative
import Data.Semigroup((<>))
import Lambda
import System.Console.Repline
import Control.Monad.Trans

data Options = Options {
  optRepl :: Bool,
  optVerbose :: Bool,
  optExpression :: Maybe String,
  optFile :: Maybe String
}

options = Options
  <$> switch
    (long "repl" <> short 'r' <> help "REPL mode")
  <*> switch
    (long "verbose" <> short 'v' <> help "Also show parsed expression")
  <*> optional (strArgument (metavar "EXPRESSION"))
  <*> optional (strOption (long "file" <> short 'f' <> help "File to execute"))

opts = info (options <**> helper) (fullDesc <> progDesc "Evaluate lambda expression" <> header "slambda - Simple Lambda Calculus")

main :: IO ()
main = app =<< execParser opts

app opts =
  if optRepl opts then do
    let execute = if optVerbose opts then verboseExecuteString else executeString
    evalRepl "> " (liftIO . execute "REPL") [] (Word $ const $ return []) (return ())
  else
    case optExpression opts of
      Just expr -> (if optVerbose opts then verboseExecuteString else executeString) "command line" expr
      _ -> case optFile opts of
        Just fileName -> do
          content <- readFile fileName
          (if optVerbose opts then verboseExecuteMultilineString else executeMultilineString) fileName content
        _ -> putStrLn "No expression given"
