import Term
import Pos
import Pretty
import Match

import Data.Maybe(isJust, isNothing)

testTargets :: [(Bool)]
testTargets = [
    testReplace,
    testReplace2,
    testAllPos1,
    testPretty,
    testSelectAt1,
    testSelectAt2,
    testSelectAt3,
    testSelectAt4,
    testSelectAt5,
    testSelectAt6
  ]

testAll :: IO ()
testAll = doTest testTargets
  where
    doTest []     = putStrLn("All tests passed!")
    doTest (t:ts) = if t == True then doTest ts else putStrLn "Test failed"

testEntity :: Term
testEntity = Comb "+" [Var "x", Var "y"]
testEntity2 :: Term
testEntity2 = Comb "+" [Comb "*" [Var "x", Var "z"], Var "y"]

testReplace :: Bool
testReplace = (replaceAt testEntity [0] (Var "z")) == Comb "+" [Var "z", Var "y"]
testReplace2 :: Bool
testReplace2 = (replaceAt testEntity2 [0, 1] (Var "Zzz")) == Comb "+" [Comb "*" [Var "x", Var "Zzz"], Var "y"]

testAllPos1 :: Bool
testAllPos1 = (allPos testEntity2) == [[],[1],[1,1],[1,2],[2]]

testPretty :: Bool
testPretty = pretty (Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Var "m", Var "n"]]) == "add (Succ Zero) (mul m n)"
testMatch :: Bool
testMatch = isJust (match testEntity testEntity2)

-- selectAt
testSelectAt1 :: Bool
testSelectAt1 = selectAt (Var "x") [] == Var "x"

testSelectAt2 :: Bool
testSelectAt2 = selectAt (Comb "abs" [(Var "x")]) [] == Comb "abs" [(Var "x")]

testSelectAt3 :: Bool
testSelectAt3 = selectAt (Comb "abs" [(Var "x")]) [1] == Var "x"

testSelectAt4 :: Bool
testSelectAt4 = isNothing (maybeSelectAt (Comb "abs" [(Var "x")]) [2])

testSelectAt5 :: Bool
testSelectAt5 = isNothing (maybeSelectAt (Var "x") [1])

testSelectAt6 :: Bool
testSelectAt6 = isNothing (maybeSelectAt (Var "x") [1])