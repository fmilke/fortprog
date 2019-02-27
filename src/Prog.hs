module Prog
  ( Lhs, Rhs, Rule(..), Prog(..)
  ) where

import Term

-- Alias type for left-hand sides.
-- A left-hand side can be represented as a term.
type Lhs = Term

-- Alias type for right-hand sides.
-- A right-hand side can be represented as a term.
type Rhs = Term

-- Data type for function rules.
-- A function rule consists of a left-hand side and a right-hand side.
data Rule = Rule Lhs Rhs
  deriving Show

-- Data type for programs.
-- A program consists of a list of function rules.
data Prog = Prog [Rule]
  deriving Show
