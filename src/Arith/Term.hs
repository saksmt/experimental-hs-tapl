module Arith.Term where

data Term = TIsZero Term | TZero | TSucc Term | TPred Term | TTrue | TFalse | TIf Term Term Term deriving Show

isValue TZero = True
isValue (TSucc term) = isValue term
isValue TTrue = True
isValue TFalse = True
isValue _ = False

data MachineState = MachineState Term | MachineFinalState Term

reduceStep :: MachineState -> MachineState
reduceStep (MachineState (TPred (TSucc x))) = MachineState x
reduceStep (MachineState (TIsZero term)) = MachineState $ case term of
  TZero -> TTrue
  _ -> TFalse
reduceStep (MachineState (TIf g a b)) = case g of
  TTrue -> MachineState a
  TFalse -> MachineState b
  _ -> case reduceStep (MachineState g) of
              MachineState term -> MachineState $ TIf term a b
              MachineFinalState term -> MachineFinalState $ TIf term a b
reduceStep (MachineState term) = MachineFinalState term
reduceStep state@(MachineFinalState _) = state

reduce state@(MachineFinalState _) = state
reduce state@(MachineState _) = reduce $ reduceStep state

term2Int :: Term -> Maybe Int -> Maybe Int
term2Int TZero (Just v) = Just v
term2Int (TSucc term) (Just v) = term2Int term $ Just $ v + 1
term2Int term v = Nothing

showTerm TTrue = "true"
showTerm TFalse = "false"
showTerm TZero = "0"
showTerm term@(TSucc _) = maybe (show term) show $ term2Int term $ Just 0
showTerm term = show term

eval :: Term -> String
eval term = case reduce $ MachineState term of
  MachineFinalState result -> showTerm result
  MachineState t -> "Fucked up on term " ++ show t

zero = TZero
suc = TSucc
pre = TPred
iff = TIf
iszero = TIsZero
tru = TTrue
fls = TFalse
