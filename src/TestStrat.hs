import Strategy
import Term
import Rule
import Prog
import Test(testMultiple)

tests :: [Bool]
tests = [
    testLOStrat,
    testROStrat,
    testLIStrat,
    testRIStrat
  ]

testAll :: IO()
testAll = testMultiple tests

prog :: Prog
prog = Prog [
    Rule (Comb "square" [Var "x"]) (Comb "*" [Var "x", Var "x"]),
    Rule (Comb "sum" [Var "x", Var "y"]) (Comb "+" [Var "x", Var "y"])
  ]

term :: Term
term = Comb "sum" [Comb "square" [Var "x"], Comb "square" [Var "y"]]

testLOStrat = loStrategy prog term == [[], [1], [2]]
testROStrat = roStrategy prog term == [[], [2], [1]]
testLIStrat = liStrategy prog term == [[1], [2], []]
testRIStrat = riStrategy prog term == [[2], [1], []]