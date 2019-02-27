module Pos(
  above,
  below,
  leftOf,
  rightOf,
  selectAt,
  replaceAt,
  allPos
) where

import Term
import Utils(mapNth)

-- t(abs(x), y):
-- Pos(x) = 0 : 0 : []
-- Pos(y) = 1 : []
type Pos = [Int]

-- a is above b and in the same 'sub-tree'
above :: Pos -> Pos -> Bool
above (a:as) (b:bs) = if a == b then above as bs else False

-- a is above b and in the same 'sub-tree'
below :: Pos -> Pos -> Bool
below (a:as) (b:bs) = if a == b then below as bs else False

-- a is on the left of b and in the same 'sub-tree
leftOf :: Pos -> Pos -> Bool
leftOf [a] [b] = a < b
leftOf (a:as) (b:bs) = if a == b then leftOf as bs else False

-- a is on the right of b and in the same 'sub-tree
rightOf :: Pos -> Pos -> Bool
rightOf [a] [b] = a > b
rightOf (a:as) (b:bs) = if a == b then rightOf as bs else False

-- selects the sub-term at the given position
selectAt :: Term -> Pos -> Term
selectAt (Comb n (t:ts)) (0:ps) = selectAt (Comb n ts) ps
selectAt (Comb n (t:ts)) (p:ps) = selectAt (Comb n ts) ((p - 1):ps)

{-|
  @Term: term to replace in
  @Pos: position of sub term to replace
  @Term: new sub term
  @Term: term with replaced sub term
-}
replaceAt :: Term -> Pos -> Term -> Term
replaceAt t1 [] t2 = t2
replaceAt (Comb n ts) (p:ps) t2 = Comb n (
    mapNth (\subT -> (replaceAt subT ps t2)) ts p
  )

-- returns all possible position of sub-terms in a given term
allPos :: Term -> [Pos]
allPos (Var _)  = [[]]
allPos (Comb _ ts) = iter ts 0
  where
    iter []     n = []
    iter (t:ts) n = (map (\p -> n : p) (allPos t)) ++ (iter ts (n + 1))