module Rule where

import Data.Maybe(isNothing)
import Pos
import Match
import Prog
import Subst
import Term

-- does stuff
findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])     _  = Nothing
findRule (Prog ((Rule lhs rhs):rs)) t = case match t lhs of
  Nothing    -> Nothing
  Just subst -> Just (rhs, subst)

-- returns Nothing
-- if not able to reduce at the given position
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt prog t pos = let subTerm = selectAt t pos in
  case findRule prog subTerm of
    Nothing           -> Nothing
    Just (rhs, subst) -> Just (replaceAt t pos rhs)

-- reduciblePos :: Prog -> Term -> [Pos]
reduciblePos prog t = filter predicate (allPos t)
  where
    predicate pos = case reduceAt prog t pos of
      Nothing -> False
      Just _  -> True

-- we call a term of normal form
-- if there is no rule left matching it
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog t = isNothing (findRule prog t)