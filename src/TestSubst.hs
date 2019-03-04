import Term
import Subst
import Test(testMultiple)

tests :: [Bool]
tests = [
    testIdentity,
    testSingle1,
    testSingle2,
    testCompose1,
    testCompose2
  ]

testAll :: IO ()
testAll = testMultiple tests

testIdentity :: Bool
testIdentity = identity "x" == Var "x"

testSingle1 :: Bool
testSingle1 = apply (single "x" (Var "y")) (Var "x") == Var "y"

testSingle2 :: Bool
testSingle2 = apply (single "x" (Var "y")) (Comb "id" [Var "x"]) == Comb "id" [Var "y"]

composed :: Subst
composed = compose (single "x" (Var "y")) (single "y" (Var "z"))

testCompose1 :: Bool
testCompose1 = apply composed (Var "x") == Var "z"

testCompose2 :: Bool
testCompose2 = apply composed (Var "x") == apply (single "y" (Var "z")) (apply (single "x" (Var "y")) (Var "x"))