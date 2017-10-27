module Main where

import Options.Applicative
import Data.Semigroup((<>))
import Lambda
import System.Console.Repline
import Control.Monad.Trans

data Options = Options {
  optRepl :: Bool,
  optVerbose :: Bool,
  optExpression :: Maybe String
}

options = Options
  <$> switch
    (long "repl" <> short 'r' <> help "REPL mode")
  <*> switch
    (long "verbose" <> short 'v' <> help "Also show parsed expression")
  <*> optional (strArgument (metavar "EXPRESSION"))

opts = info (options <**> helper) (fullDesc <> progDesc "Evaluate lambda expression" <> header "slambda - Simple Lambda Calculus")

main :: IO ()
main = app =<< execParser opts

app opts = do
  let execute = if optVerbose opts then verboseExecuteString else executeString
  if optRepl opts then
    evalRepl "> " (liftIO . execute "REPL") [] (Word $ const $ return []) (return ())
  else
    case optExpression opts of
      Just expr -> execute "command line" expr
      _ -> putStrLn "No expression given"
