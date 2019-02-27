module Term
  ( VarName, CombName, Term(..)
  ) where

-- Alias type for variable names.
-- A variable name is a string.
type VarName = String

-- Alias type for constructor and function names.
-- A constructor or function name is a string.
type CombName = String

-- Data type for terms.
-- A term is either a variable,
-- or a constructor or function applied to a list of terms.
data Term = Var VarName | Comb CombName [Term]
  deriving (Eq, Show)