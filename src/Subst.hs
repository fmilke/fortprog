module Subst(
  identity,
  single,
  compose,
  apply,
  Subst
) where

import Term

type Subst = (Term -> Term)

identity :: Subst
identity t = t

single:: VarName -> Term -> Subst
  single vn t1 = \t2 -> if t2 == Var vn then t1
    else t2

compose :: Subst -> Subst -> Subst
compose s1 s2 = \t -> s2 (s1 t)

apply :: Subst -> Term -> Term
apply subst (Comb name ts) = Comb name (map (apply subst) ts)
apply subst t = subst t
