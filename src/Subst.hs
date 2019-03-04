module Subst(
  identity,
  single,
  compose,
  apply,
  Subst
) where

import Term

type Subst = (VarName -> Term)

identity :: Subst
identity vn = Var vn

single :: VarName -> Term -> Subst
single vn t1 = \vn2 -> if vn == vn2 then t1
  else Var vn2

compose :: Subst -> Subst -> Subst
compose s1 s2 = \vn -> apply s2 (s1 vn)

apply :: Subst -> Term -> Term
apply subst (Var name) = subst name
apply subst (Comb name ts) = Comb name (map (apply subst) ts) 