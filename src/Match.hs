module Match (
  match
) where

import Subst
import Term

-- case1: match
-- t1(x)
-- t2(t3)

-- case2: match?
-- t1(x)
-- t2(y)

-- case3: match
-- t1(t3)
-- t2(t3)

-- case4: no match !
-- t1(t3)
-- t2(t4)

-- case5: no match !
-- t1(t3)
-- t2(z)

match :: Term -> Term -> Maybe Subst
match t1 t2 = step t1 t2 (Just identity)
-- where
step :: Term -> Term -> Maybe Subst -> Maybe Subst
-- case5:
step (Comb _ _) (Var _) _     = Nothing
-- case1:
-- case2:
step (Var n) t (Just subst)   = (Just (compose (single n t) subst))
step (Comb n (t:ts)) (Comb m (r:rs)) (Just subst)
  -- case3:
  | n == m    = (step (Comb n ts) (Comb n rs) (step t r (Just subst)))
  -- case4:
  | otherwise = Nothing
step _ _ Nothing = Nothing