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

single :: VarName -> Term -> Subst
single x t = singleLambda x t
  where
    singleLambda :: VarName -> Term -> Term -> Term
    singleLambda x t (Var y) = if x == y then t else Var y

compose :: Subst -> Subst -> Subst
compose s1 s2 = \t -> s2 (s1 t)

apply :: Subst -> Term -> Term
apply subst (Comb name ts) = Comb name (map (apply subst) ts)
apply subst t = subst t