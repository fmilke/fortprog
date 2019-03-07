module Strategy where

import Pos
import Rule
import Term
import Prog

import Data.List(sortBy)

-- Alias type for evaluation strategies
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy prog term = case sortBy comp (reduciblePos prog term) of
  [] -> []
  ps -> [head ps]
  where
    comp a b | a `above`b   = LT
             | a `leftOf` b = LT
             | otherwise    = GT

liStrategy :: Strategy
liStrategy prog term = case sortBy comp (reduciblePos prog term) of
  [] -> []
  ps -> [head ps]
  where
    comp a b | a `leftOf` b = LT
             | a `below` b  = LT
             | otherwise    = GT

roStrategy :: Strategy
roStrategy prog term = case sortBy comp (reduciblePos prog term) of
  [] -> []
  ps -> [head ps]
  where
    comp a b | a `above` b   = LT
             | a `rightOf` b = LT
             | otherwise     = GT

riStrategy :: Strategy
riStrategy prog term = case sortBy comp (reduciblePos prog term) of
  [] -> []
  ps -> [head ps]
  where
    comp a b | a `rightOf` b = LT
             | a `below` b   = LT
             | otherwise     = GT

poStrategy :: Strategy
poStrategy prog term = let sorted = sortBy comp (reduciblePos prog term) in
  filter (belowAll sorted) sorted
    where
      comp a b | a `above` b = LT
               | otherwise   = GT
      belowAll []     _ = True
      belowAll (a:as) e = if e == a || e `above` a then belowAll as e else False

piStrategy :: Strategy
piStrategy prog term = let sorted = reduciblePos prog term in
  filter (belowAll sorted) sorted
    where
      belowAll []     _ = True
      belowAll (a:as) e = if e == a || e `below` a then belowAll as e else False

loStrategy :: Strategy
loStrategy prog term = remove lmPos above lmPos where
  lmPos = remove redPos leftOf redPos where
    redPos = reduciblePos prog term


liStrategy :: Strategy
liStrategy prog term = remove lmPos below lmPos where
  lmPos = remove redPos leftOf redPos where
    redPos = reduciblePos prog term

roStrategy :: Strategy
roStrategy prog term = remove rmPos above rmPos where
  rmPos = remove redPos rightOf redPos where
    redPos = reduciblePos prog term

riStrategy :: Strategy
riStrategy prog term = remove rmPos below rmPos where
  rmPos = remove redPos rightOf redPos where
    redPos = reduciblePos prog term

poStrategy :: Strategy
poStrategy prog term = remove redPos above redPos where
  redPos = reduciblePos prog term

piStrategy :: Strategy
piStrategy prog term = remove redPos below redPos where
  redPos = reduciblePos prog term

-- removes all positions from the seccond list that are in relation to a postition from the first list
-- rel should not be reflexive
remove :: [[Int]] -> ([Int] ->[Int] -> Bool) -> [[Int]] -> [[Int]]
remove [] rel ps = ps
remove (v:vs) rel ps = remove vs rel (filter (\p -> not(rel v p)) ps)

-- perform a single reduction of
-- a given term with the given strategy
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog t = case strat prog t of
    [] -> Nothing
    ps -> foldr (\p newTerm -> do {
      term <- newTerm ;
      reduceAt prog term p }) (Just t) ps

-- evaluates a given term with the given
-- program and strategy until no more
-- reduction is possible
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog t = case reduceWith strat prog t of
  Nothing -> t
  Just rt -> evaluateWith strat prog rt