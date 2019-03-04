import Test(testMultiple)
import Rule
import Prog
import Term
import Data.Maybe(isNothing)

tests :: [Bool]
tests = [
    testFindRule1,
    testFindRule2
  ]

testAll :: IO ()
testAll = testMultiple tests

negLhs :: Term
negLhs = Comb "neg" [Var "x"]
negRhs :: Term
negRhs = Comb "-" [Var "x"]

squareLhs :: Term
squareLhs = Comb "square" [Var "x"]
squareRhs :: Term
squareRhs = Comb "*" [Var "x", Var "x"]

prog :: Prog
prog = Prog [Rule negLhs negRhs, Rule squareLhs squareRhs]

testFindRule1 :: Bool
testFindRule1 = case findRule prog squareLhs of
  Just (rhs, _) -> rhs == squareRhs
  Nothing -> False

testFindRule2 :: Bool
testFindRule2 = isNothing (findRule prog (Comb "++" [Var "x"]))

-- reduce
