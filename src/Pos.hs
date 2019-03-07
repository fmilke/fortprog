module Pos(
  above,
  below,
  leftOf,
  rightOf,
  selectAt,
  maybeSelectAt,
  replaceAt,
  allPos,
  Pos
) where

import Term

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
selectAt t               []     = t
selectAt (Var n)         (_:_)  = error ("Erronously accessing sub-terms of Var: " ++ n)
selectAt (Comb n [])         _  = error ("Erronously accessing sub-terms of childless term: " ++ n)
selectAt (Comb _ (t:ts)) (p:ps) 
  | p == 1    = selectAt t ps
  | p <  1    = error "Invalid position"
  | otherwise = selectAt (Comb "" ts) ((p - 1):ps)

-- this is a more secure version of selectAt
-- and therefore is more suitable for tests
maybeSelectAt :: Term -> Pos -> Maybe Term
maybeSelectAt t               []     = Just t
maybeSelectAt (Var _)         (_:_)  = Nothing
maybeSelectAt (Comb _ [])         _  = Nothing
maybeSelectAt (Comb _ (t:ts)) (p:ps) 
  | p == 1    = maybeSelectAt t ps
  | p <  1    = Nothing
  | otherwise = maybeSelectAt (Comb "" ts) ((p - 1):ps)

{-|
  @Term: term to replace in
  @Pos: position of sub term to replace
  @Term: new sub term
  @Term: term with replaced sub term
-}
replaceAt :: Term -> Pos -> Term -> Term
replaceAt _ [] t2               = t2
replaceAt (Var n) (_:_) _       = error ("Trying to replace child of Var: " ++ n)
replaceAt (Comb n ts) (p:ps) t2 = Comb n (
    mapNth (\subT -> (replaceAt subT ps t2)) ts p
  )

-- map like function, but only applies to the nth element
mapNth :: (a -> a) -> [a] -> Int -> [a]
mapNth fn (a:as) 1 = (fn a) : as
mapNth _  []     _ = error ("Access out of bound index")
mapNth fn (a:as) n = a : (mapNth fn as (n - 1))

-- returns all possible position of sub-terms in a given term
allPos :: Term -> [Pos]
allPos (Var _)  = [[]]
allPos (Comb _ xs) = [] : iter xs 1
  where
    iter []     _ = []
    iter (t:ts) n = (map (\p -> n : p) (allPos t)) ++ (iter ts (n + 1))