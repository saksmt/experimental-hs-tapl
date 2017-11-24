module Lambda.Reduce.Show
    ( showTerm
    , showAlphaTerm
    , dumpScope
    , dumpScope'
    , dumpContext
    ) where

import Lambda.Term
import Lambda.Reduce.Alpha
import Data.List(intercalate)

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

showAlphaTerm (AAbstraction name body) = "λ" ++ showAlphaTerm body
showAlphaTerm (AApplication body param) = "(" ++ (showAlphaTerm body) ++ ") (" ++ (showAlphaTerm param) ++ ")"
showAlphaTerm (AVariable name index) = " " ++ show index
showAlphaTerm (AUnresolvedVariable name) = " " ++ name
showAlphaTerm (AUndefined term) = showAlphaTerm term

dumpContext (cutoff, vars) = foldl1 (++) $
    [ "Global cutoff: "
    , show cutoff
    , "\n"
    ] ++ varDump
    where varDump = map (\(k, v) -> show k ++ " == " ++ showAlphaTerm v ++ " // " ++ (showTerm $ fromAlpha v) ++ "\n") $ zip [0..] $ map alphaVarValue vars

-- Predicates

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
