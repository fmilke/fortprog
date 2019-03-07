module Subst(
  identity,
  single,
  compose,
  apply,
  Subst
) where

import Term

-- data type for replacing
-- vars in a term with another term
type Subst = (VarName -> Term)

-- replace a var by itself
identity :: Subst
identity vn = Var vn

-- replace a var of a given name
-- with the given term
single :: VarName -> Term -> Subst
single vn t1 = \vn2 -> if vn == vn2 then t1
  else Var vn2

-- aggregates two substitutions
compose :: Subst -> Subst -> Subst
compose s1 s2 = \vn -> apply s2 (s1 vn)

-- applies a substituion to
-- a term and all its descendants
apply :: Subst -> Term -> Term
apply subst (Var name) = subst name
apply subst (Comb name ts) = Comb name (map (apply subst) ts) 