module Strategy (
  loStrategy,
  liStrategy,
  roStrategy,
  riStrategy,
  poStrategy,
  piStrategy,
  reduceWith,
  evaluateWith,
  Strategy
) where

import Pos
import Rule
import Term
import Prog

-- Alias type for evaluation strategies
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy prog term = remove lmPos above lmPos
  where
    lmPos = remove redPos leftOf redPos
      where
        redPos = reduciblePos prog term

liStrategy :: Strategy
liStrategy prog term = remove lmPos below lmPos 
  where
    lmPos = remove redPos leftOf redPos
      where
        redPos = reduciblePos prog term

roStrategy :: Strategy
roStrategy prog term = remove rmPos above rmPos
  where
    rmPos = remove redPos rightOf redPos 
      where
        redPos = reduciblePos prog term

riStrategy :: Strategy
riStrategy prog term = remove rmPos below rmPos 
  where
    rmPos = remove redPos rightOf redPos 
      where
        redPos = reduciblePos prog term

poStrategy :: Strategy
poStrategy prog term = remove redPos above redPos 
  where
    redPos = reduciblePos prog term

piStrategy :: Strategy
piStrategy prog term = remove redPos below redPos 
  where
    redPos = reduciblePos prog term

-- removes all positions from the seccond list
-- that are in relation to a postition from the first list
-- rel should not be reflexive
remove :: [[Int]] -> ([Int] ->[Int] -> Bool) -> [[Int]] -> [[Int]]
remove []     _   ps = ps
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
