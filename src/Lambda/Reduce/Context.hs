module Lambda.Reduce.Context
    ( pushVariable
    , globalCutoff
    , extractGlobal
    , globalNames
    , contextFrom
    , get
    ) where

import Lambda.Term
import Lambda.Reduce.Alpha

pushVariable :: AlphaContext -> AlphaVariable -> AlphaContext
pushVariable (globalCutoff, vars) var = (globalCutoff, vars ++ [var])

globalCutoff (c, _) = c
extractGlobal (cutoff, vars) = (cutoff, take cutoff $ vars)
globalNames (cutoff, vars) = map alphaVarGlobalName $ filter isGlobalVar $ take cutoff $ vars

contextFrom scope = ((length alphaVars), alphaVars)
    where flattened = toList scope
          names = map getVarName $ map fst $ flattened
          alphaVars = map (\(VariableName name, value) -> AlphaVarGlobalValue { alphaVarGlobalName = name, alphaVarValue = toAlpha names value }) flattened

isGlobalVar (AlphaVarGlobalValue _ _) = True
isGlobalVar _ = False

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (v:_) = Just v

(??) :: [a] -> Int -> Maybe a
list ?? idx = snd <$> (safeHead $ filter ((==idx) . fst) $ zip [0..] list)

get :: AlphaContext -> Int -> Maybe AlphaVariable
get (_, vars) index = vars ?? index
