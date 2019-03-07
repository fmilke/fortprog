module Strategy where

import Pos
import Rule
import Term
import Prog

import Data.List(sortBy)

-- Alias type for evaluation strategies
type Strategy = Prog -> Term -> [Pos]

-- loStrategy :: Strategy
-- loStrategy prog term = case sortBy comp (reduciblePos prog term) of
--   [] -> []
--   ps -> [head ps]
--   where
--     comp a b | a `above`b   = LT
--              | a `leftOf` b = LT
--              | otherwise    = GT

-- liStrategy :: Strategy
-- liStrategy prog term = case sortBy comp (reduciblePos prog term) of
--   [] -> []
--   ps -> [head ps]
--   where
--     comp a b | a `leftOf` b = LT
--              | a `below` b  = LT
--              | otherwise    = GT

-- roStrategy :: Strategy
-- roStrategy prog term = case sortBy comp (reduciblePos prog term) of
--   [] -> []
--   ps -> [head ps]
--   where
--     comp a b | a `above` b   = LT
--              | a `rightOf` b = LT
--              | otherwise     = GT

-- riStrategy :: Strategy
-- riStrategy prog term = case sortBy comp (reduciblePos prog term) of
--   [] -> []
--   ps -> [head ps]
--   where
--     comp a b | a `rightOf` b = LT
--              | a `below` b   = LT
--              | otherwise     = GT

-- poStrategy :: Strategy
-- poStrategy prog term = let sorted = sortBy comp (reduciblePos prog term) in
--   filter (belowAll sorted) sorted
--     where
--       comp a b | a `above` b = LT
--                | otherwise   = GT
--       belowAll []     _ = True
--       belowAll (a:as) e = if e `below` a then belowAll as e else False

-- piStrategy :: Strategy
-- piStrategy = poStrategy

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