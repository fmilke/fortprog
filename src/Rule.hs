module Rule where

import Data.Maybe(isNothing)
import Pos
import Match
import Prog
import Subst
import Term

-- finds a rule matching the given term (root-level only)
findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])     _  = Nothing
findRule (Prog ((Rule lhs rhs):rs)) t = case match lhs t of
  Nothing    -> findRule (Prog rs) t
  Just subst -> Just (rhs, subst)

-- tries to reduce at a given position
-- returns nothing if no matching rule is foudn
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt prog t pos = let subTerm = selectAt t pos in
  case findRule prog subTerm of
    Nothing           -> Nothing
    Just (rhs, subst) -> Just (replaceAt t pos (apply subst rhs))

-- returns all position where a rule for
-- reduction exists
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos prog t = filter reducible (allPos t)
  where
    reducible pos = case reduceAt prog t pos of
      Nothing -> False
      Just _  -> True

-- we call a term of normal form
-- if there is no rule left matching it
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog t = reduciblePos prog t == []