import Term
import Pos

-- tests
testEntity = Comb "+" [Var "x", Var "y"]
testEntity2 = Comb "+" [Comb "*" [Var "x", Var "z"], Var "y"]

--  should be Comb "+" [Var "z", Var "y"]
testReplace = (replaceAt testEntity [0] (Var "z"))
--  should be Comb "+" [Comb "*" [Var "x", Var "Zzz"], Var "y"]
testReplace2 = (replaceAt testEntity2 [0, 1] (Var "Zzz"))

-- should be [[0,0],[0,1],[1]]
testAllPos1 = allPos testEntity2