module Lambda(evalString, verboseEvalString, executeString, verboseExecuteString) where

import Lambda.Term
import Lambda.Parse
import Lambda.Reduce
import qualified Text.Parsec as P
import Control.Applicative
import Data.Maybe

_evalString name v evaluate = either show doEval $ P.parse tokensP name v
  where doEval t = maybe "PARSE ERROR" evaluate (parse t)

evalString name v = _evalString name v eval
verboseEvalString name v = _evalString name v verboseEval
executeString name = putStrLn . evalString name
verboseExecuteString name = putStrLn . verboseEvalString name
