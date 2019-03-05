module Strategy where

import Pos
import Rule
import Term
import Prog

import Data.List(sortBy)

-- Alias type for evaluation strategies
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy prog term = sortBy comp (reduciblePos prog term)
  where
    comp a b | a `above` b  = LT
             | a `leftOf` b = LT
             | otherwise    = GT

liStrategy :: Strategy
liStrategy prog term = sortBy comp (reduciblePos prog term)
  where
    comp a b | a `below` b  = LT
             | a `leftOf` b = LT
             | otherwise    = GT

roStrategy :: Strategy
roStrategy prog term = sortBy comp (reduciblePos prog term)
  where
    comp a b | a `above` b   = LT
             | a `rightOf` b = LT
             | otherwise     = GT

riStrategy :: Strategy
riStrategy prog term = sortBy comp (reduciblePos prog term)
  where
    comp a b | a `below` b   = LT
             | a `rightOf` b = LT
             | otherwise     = GT

-- poStrategy :: Strategy
-- piStrategy :: Strategy

-- perform a single reduction of
-- a given term with the given strategy
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog t = case strat prog t of
  []     -> Nothing
  (p:_) -> reduceAt prog t p

-- evaluates a given term with the given
-- program and strategy until no more
-- reduction is possible
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog t = case reduceWith strat prog t of
  Nothing -> t
  Just rt -> evaluateWith strat prog rt