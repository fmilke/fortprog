module Peano where

data Peano = Zero | Succ Peano

add :: Peano -> Peano -> Peano
add Zero     m = m
add (Succ n) m = Succ (add n m)

mul :: Peano -> Peano -> Peano
mul Zero     m = Zero
mul (Succ n) m = add (mul n m) m

double :: Peano -> Peano
double x = add x x

square :: Peano -> Peano
square x = mul x x
