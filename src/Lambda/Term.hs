module Lambda.Term where

newtype VariableName = VariableName String deriving (Eq, Show)
data Scope = Scope { parent :: Maybe Scope, variables :: [(VariableName, Term)] } deriving (Show)
data Term = TApplication Term Term | TAbstraction VariableName Term | TVarRef VariableName deriving (Show)
data MachineState = MachineState Scope Term | MachineFinalState Scope Term
