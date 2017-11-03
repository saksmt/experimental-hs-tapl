module Lambda.Term where

import Data.Semigroup((<>), Semigroup)

newtype VariableName = VariableName String deriving (Eq, Show)
data Scope = Scope { parent :: Maybe Scope, variables :: [(VariableName, Term)] } deriving (Show)
data Term = TApplication Term Term | TAbstraction VariableName Term | TVarRef VariableName | TUndefined Term deriving (Show)
data MachineState = MachineState Scope Term | MachineFinalState Scope Term

instance Semigroup Scope

instance Monoid Scope where
  mempty = Scope { parent = mempty, variables = mempty }
  mappend (Scope p1 v1) (Scope p2 v2) = Scope { parent = p1 <> p2, variables = v1 <> v2 }
