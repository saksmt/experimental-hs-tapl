module Lambda
  (evalString
  , verboseEvalString
  , executeString
  , verboseExecuteString
  , evalMultilineString
  , executeMultilineString
  , verboseEvalMultilineString
  , verboseExecuteMultilineString
  ) where

import Lambda.Term
import Lambda.Parse
import Lambda.Reduce
import qualified Text.Parsec as P
import Control.Applicative
import Data.Maybe

_evalString name v evaluate = either show (evaluate mempty) $ P.parse exprP name v
_evalMultilineString name v evaluate = either show doEvaluate $ P.parse fileP name v
  where doEvaluate s = fromMaybe (noMainMessage s) $ evaluate s <$> maybeGetVar "main" s
        noMainMessage s = showParsed s ++ "\nNo main function found. Nothing to reduce."

evalString name v = _evalString name v evalToString
verboseEvalString name v = _evalString name v verboseEvalToString
executeString name = putStrLn . evalString name
verboseExecuteString name = putStrLn . verboseEvalString name

showParsed s = concat
  ["Parsed:\n\n"
  , unlines $ map (\ l -> ' ' : ' ' : ' ' : ' ' : l) $ lines $ dumpScope s
  ]

evalMultilineString name v = _evalMultilineString name v evalToString
executeMultilineString name = putStrLn . evalMultilineString name
verboseEvalMultilineString name v = _evalMultilineString name v doEval
  where doEval s t = concat
                    [ showParsed s
                    , "\nMain function reduction result: "
                    , evalToString s t
                    ]
verboseExecuteMultilineString name = putStrLn . verboseEvalMultilineString name
