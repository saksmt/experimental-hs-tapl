module Lambda.Reduce(eval, verboseEval) where

import Lambda.Term
import Control.Applicative

getVar name scope = case localVariable <|> parentVariable of
    Just term -> term
    _ -> error $ name ++ " variable not in scope"
  where localVariable = VariableName name `lookup` variables scope
        parentVariable = getVar name <$> parent scope

reduceStep :: MachineState -> MachineState
reduceStep (MachineState scope (TVarRef (VariableName name))) = MachineState scope (getVar name scope)
reduceStep (MachineState scope (TApplication (TAbstraction name term) variable)) = MachineState (Scope (Just scope) [(name, variable)]) term
reduceStep (MachineState scope (TApplication t1 t2)) = case reduceStep $ MachineState scope t1 of
  MachineState scope' term -> MachineState scope' $ TApplication term  t2
  MachineFinalState scope' term -> MachineFinalState scope' $ TApplication term t2
reduceStep (MachineState scope term@(TAbstraction _ _)) = MachineFinalState scope term
reduceStep machine@(MachineFinalState _ _) = machine

reduce state@(MachineFinalState scope _) = state
reduce state@(MachineState scope _) = reduce $ reduceStep state

showTerm (TAbstraction (VariableName name) term) = "λ " ++ name ++ ". " ++ showTerm term
showTerm (TApplication t1 t2) = showTerm t1 ++ " " ++ showTerm t2
showTerm (TVarRef (VariableName name)) = name

emptyScope = Scope Nothing []
eval term = case reduce $ MachineState emptyScope term of
  MachineFinalState _ term -> showTerm term
  MachineState _ _ -> "FUCKUP"

verboseEval term = showTerm term ++ "  ⟶*  " ++ eval term
