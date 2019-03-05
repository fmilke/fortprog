import Test(testMultiple)
import Rule
import Prog
import Term
import Pos
import Match
import Subst
import Data.Maybe(isNothing)

tests :: [Bool]
tests = [
    testFindRule1,
    testFindRule2,
    testFindRule3,
    testFindRule4,
    testReduceAt
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

-- test findRule for success
testFindRule1 :: Bool
testFindRule1 = case findRule prog squareLhs of
  Just (rhs, _) -> rhs == squareRhs
  Nothing       -> False

-- test malicous findRule
testFindRule2 :: Bool
testFindRule2 = isNothing (findRule prog (Comb "++" [Var "x"]))

-- test findRule for correct-ish subsitution
testFindRule3 :: Bool
testFindRule3 = case findRule prog (Comb "square" [Var "y"]) of
  Just (rhs, subst) -> apply subst rhs == Comb "*" [Var "y", Var "y"] 
  Nothing          -> error "something wrong wih the test setup"

-- square (neg y) shall become (neg y) * (neg y)
testFindRule4 :: Bool
testFindRule4 = case findRule prog (Comb "square" [Comb "neg" [Var "y"]]) of
  Just (rhs, subst) -> apply subst rhs == Comb "*" [Comb "neg" [Var "y"], Comb "neg" [Var "y"]] 
  Nothing          -> error "something wrong wih the test setup"

-- reduce
testReduceAt :: Bool
testReduceAt = case (reduceAt prog (Comb "square" [Var "y"]) []) of
  Just res ->  res == Comb "*" [Var "y", Var "y"]
  Nothing  -> error "something wrong wih the test setup"