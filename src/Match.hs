module Match (
  match
) where

import Subst
import Term

match :: Term -> Term -> Maybe Subst
match t1 t2 = step t1 t2
  where
    step (Var n) t subs = 