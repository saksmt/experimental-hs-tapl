module Lambda.Reduce.Alpha(fromAlpha, toAlpha) where

import Lambda.Term
import Data.List(elemIndex)

fromAlpha (AAbstraction name term) = TAbstraction (VariableName name) $ fromAlpha term
fromAlpha (AApplication body param) = TApplication (fromAlpha body) (fromAlpha param)
fromAlpha (AUnresolvedVariable name) = TVarRef $ VariableName name
fromAlpha (AVariable name _) = TVarRef $ VariableName name
fromAlpha (AUndefined term) = TUndefined $ fromAlpha term

toAlpha :: [String] -> Term -> AlphaTerm
toAlpha availableNames (TVarRef (VariableName name)) = maybe (AUnresolvedVariable name) (AVariable name) (name `elemIndex` availableNames)
toAlpha availableNames (TAbstraction (VariableName name) term) = AAbstraction name $ toAlpha (availableNames ++ [name]) term
toAlpha availableNames (TApplication body param) = AApplication (toAlpha availableNames body) (toAlpha availableNames param)
toAlpha availableNames (TUndefined term) = AUndefined $ toAlpha availableNames term

