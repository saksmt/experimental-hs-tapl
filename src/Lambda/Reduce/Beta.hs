module Lambda.Reduce.Beta(betaReduce) where

import Lambda.Term
import Lambda.Reduce.Context
import Data.Maybe(fromMaybe)
import Lambda.Reduce.Show
import Debug.Trace

betaApplicationBody context body param = let shiftedBody = shiftTerm (globalCutoff context) 1 body
                                             newContext = context `pushVariable` (AlphaVarValue param)
                                         in betaReduce newContext shiftedBody

betaReduce :: AlphaContext -> AlphaTerm -> AlphaTerm
betaReduce context term@(AApplication (AAbstraction name body) param) = trace (dumpContext context ++ showAlphaTerm term) $ betaApplicationBody context body param
betaReduce context term@(AApplication body param) = trace (dumpContext context ++ showAlphaTerm term) $ case betaReduce context body of
                                                   AAbstraction name bodyRedux -> betaApplicationBody context bodyRedux param
                                                   term -> AApplication term param
betaReduce context term@(AAbstraction _ _) = trace (dumpContext context ++ showAlphaTerm term) $ term
betaReduce context term@(AUndefined _) = trace (dumpContext context ++ showAlphaTerm term) $ term
betaReduce context term@(AUnresolvedVariable _) = trace (dumpContext context ++ showAlphaTerm term) $ term
betaReduce context term@(AVariable name index) = trace (dumpContext context ++ showAlphaTerm term) $ fromMaybe term $ (betaReduce (extractGlobal context)) <$> alphaVarValue <$> (context `get` index)

shiftTerm :: Int -> Int -> AlphaTerm -> AlphaTerm
shiftTerm cutoff place source@(AVariable name index)
    | index < cutoff = source
    | otherwise = AVariable name (index + place)
shiftTerm cutoff place (AAbstraction name term) = AAbstraction name $ shiftTerm (cutoff + 1) place term
shiftTerm cutoff place (AApplication body param) = AApplication (shiftTerm cutoff place body) (shiftTerm cutoff place body)
shiftTerm cutoff place (AUndefined term) = AUndefined $ shiftTerm cutoff place term
shiftTerm cutoff place term@(AUnresolvedVariable _) = term
