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

reduceStep :: MachineState -> MachineState
reduceStep (MachineState scope (TVarRef (VariableName name))) = MachineState scope (getVar name scope)
reduceStep (MachineState scope (TApplication (TAbstraction name term) variable)) = MachineState (Scope (Just scope) [(name, variable)]) term
reduceStep (MachineState scope (TApplication t1 t2)) = case reduceStep $ MachineState scope t1 of
  MachineState scope' term -> MachineState scope' $ TApplication term  t2
  MachineFinalState scope' term -> MachineFinalState scope' $ TApplication term t2
reduceStep (MachineState scope term@(TUndefined _)) = MachineFinalState scope term
reduceStep (MachineState scope term@(TAbstraction _ _)) = MachineFinalState scope term
reduceStep machine@(MachineFinalState _ _) = machine

reduce state@(MachineFinalState scope _) = state
reduce state@(MachineState scope term) = reduce $ reduceStep state

termIsSimple (TVarRef _) = True
termIsSimple (TUndefined term) = termIsSimple term
termIsSimple _ = False

termIsAbstraction (TAbstraction _ _) = True
termIsAbstraction (TUndefined term) = termIsAbstraction term
termIsAbstraction _ = False

termIsApplication (TApplication _ _) = True
termIsApplication (TUndefined term) = termIsApplication term
termIsApplication _ = False

tOr cond1 cond2 x = cond1 x || cond2 x
tAnd cond1 cond2 x = cond1 x && cond2 x
tNot cond x = not $ cond x

rightSubtermIsComplex (TApplication _ r) = not $ termIsSimple r
wrapIf cond printer term = if cond term then "(" ++ printer term ++ ")" else printer term

showTerm (TUndefined term) = showTerm term
showTerm (TAbstraction (VariableName name) term) = "λ " ++ name ++ ". " ++ wrapIf (termIsApplication `tAnd` rightSubtermIsComplex) showTerm term
showTerm (TApplication t1 t2) = doShow t1 ++ " " ++ doShow t2
  where doShow = wrapIf termP showTerm
        termP (TApplication _ r) = not $ termIsSimple r
        termP v = not $ termIsSimple v
showTerm (TVarRef (VariableName name)) = name

dumpScope (Scope p v) = intercalate "\n" $ filter (not . null) [maybe "" dumpScope p, dumpVariables]
  where dumpVariables = unlines (map dumpVariable v)
        dumpVariable (VariableName name, term) = name ++ " = " ++ showTerm term

dumpScope' parents (Scope p v) = if parents <= 0 then "" else maybe "" (dumpScope' $ parents - 1) p ++ dumpVariables
  where dumpVariables = unlines (map dumpVariable v) ++ "\n"
        dumpVariable (VariableName name, term) = name ++ " = " ++ showTerm term

eval startScope term = case reduce $ MachineState startScope term of
  MachineFinalState s term -> (term, dumpScope s)
  MachineState s stuck -> error $ concat
    ["Failed to reduce to final state. Source expression was: "
    , showTerm term
    , ". Stuck on: "
    , showTerm stuck
    , ". Context contains: "
    , dumpScope s
    ]

evalToString startScope term = showTerm $ fst $ eval startScope term

verboseEvalToString startScope term = showTerm term ++ "  ⟶*  " ++ evalToString startScope term
verboseEval startScope term = first (\x -> showTerm term ++ "  ⟶*  " ++ showTerm x) $ eval startScope term

newtype TScope = TScope ([(Int, String)], [Term])
