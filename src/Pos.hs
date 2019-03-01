module Pos(
  above,
  below,
  leftOf,
  rightOf,
  selectAt,
  replaceAt,
  allPos,
  Pos
) where

import Term
import Utils(mapNth)

-- t(abs(x), y):
-- Pos(x) = 0 : 0 : []
-- Pos(y) = 1 : []
type Pos = [Int]

  above :: Pos -> Pos -> Bool
  above [] _ = True
  above _ [] = False
  above (p:ps) (q:qs) = p == q && above ps qs
      
  below :: Pos -> Pos -> Bool
  below [] _ = False
  below _ [] = True
  below (p:ps) (q:qs) = p == q && below ps qs
  
  leftOf :: Pos -> Pos -> Bool
  leftOf [] _ = False
  leftOf _ [] = False
  leftOf (p:ps) (q:qs) = p < q || (p == q && leftOf ps qs)
    
  rightOf :: Pos -> Pos -> Bool
  rightOf [] _ = False
  rightOf _ [] = False
  rightOf (p:ps) (q:qs) = p > q || (p == q && rightOf ps qs)

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
