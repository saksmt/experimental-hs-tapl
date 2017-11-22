module Lambda.Reduce
  ( eval
  , verboseEval
  , evalToString
  , verboseEvalToString
  , showTerm
  , dumpScope
  , getVar
  , maybeGetVar
  ) where

import Lambda.Term
import Lambda.Reduce.Show
import Lambda.Reduce.Alpha
import Lambda.Reduce.Beta
import Lambda.Reduce.Context

import Control.Applicative
import Data.Bifunctor
import Debug.Trace
import Data.List

getVar name scope = case localVariable <|> parentVariable of
    Just term -> term
    _ -> TUndefined $ TVarRef $ VariableName name
  where localVariable = VariableName name `lookup` variables scope
        parentVariable = getVar name <$> parent scope

maybeGetVar name scope = case localVariable <|> parentVariable of
     Just term -> Just term
     _ -> Nothing
   where localVariable = VariableName name `lookup` variables scope
         parentVariable = getVar name <$> parent scope

eval startScope term = let startContext = contextFrom startScope
                           redux = fromAlpha $ betaReduce startContext $ toAlpha (globalNames startContext) term
                       in (redux, dumpScope startScope)

evalToString startScope term = showTerm $ fst $ eval startScope term

verboseEvalToString startScope term = showTerm term ++ "  ⟶*  " ++ evalToString startScope term
verboseEval startScope term = first (\x -> showTerm term ++ "  ⟶*  " ++ showTerm x) $ eval startScope term
