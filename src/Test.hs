import Term
import Pos
import Pretty

testTargets = [
    testReplace,
    testReplace2,
    testAllPos1,
    testPretty
  ]

testAll :: IO ()
testAll = doTest testTargets
  where
    doTest []     = putStrLn("All tests passed!")
    doTest (t:ts) = if t == True then doTest ts else putStrLn "Test failed"

testEntity = Comb "+" [Var "x", Var "y"]
testEntity2 = Comb "+" [Comb "*" [Var "x", Var "z"], Var "y"]

testReplace = (replaceAt testEntity [0] (Var "z")) == Comb "+" [Var "z", Var "y"]
testReplace2 = (replaceAt testEntity2 [0, 1] (Var "Zzz")) == Comb "+" [Comb "*" [Var "x", Var "Zzz"], Var "y"]

testAllPos1 = (allPos testEntity2) == [[0,0],[0,1],[1]]

testPretty = pretty (Comb "add" [Comb "Succ" [Comb "Zero" []], Comb "mul" [Var "m", Var "n"]]) == "add (Succ Zero) (mul m n)"