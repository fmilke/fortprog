module Bin where

data Nat = Zero | Pos Bin

data Bin = IHi | O Bin | I Bin

add :: Nat -> Nat -> Nat
add Zero    n = n
add (Pos m) n = Pos (add0 m n)

add0 :: Bin -> Nat -> Bin
add0 m Zero    = m
add0 m (Pos n) = add1 m n

add1 :: Bin -> Bin -> Bin
add1 IHi   n = add2 n
add1 (O m) n = add3 m n
add1 (I m) n = add4 m n

add2 :: Bin -> Bin
add2 IHi   = O IHi
add2 (O m) = I m
add2 (I m) = O (add2 m)

add3 :: Bin -> Bin -> Bin
add3 m IHi   = I m
add3 m (O n) = O (add1 m n)
add3 m (I n) = I (add1 m n)

add4 :: Bin -> Bin -> Bin
add4 m IHi   = O (add2 m)
add4 m (O n) = I (add1 m n)
add4 m (I n) = O (add2 (add1 m n))

mul :: Nat -> Nat -> Nat
mul Zero    _ = Zero
mul (Pos m) n = mul0 m n

mul0 :: Bin -> Nat -> Nat
mul0 m Zero    = Zero
mul0 m (Pos n) = Pos (mul1 m n)

mul1 :: Bin -> Bin -> Bin
mul1 IHi   n = n
mul1 (O m) n = O (mul1 m n)
mul1 (I m) n = add1 n (O (mul1 n m))

double :: Nat -> Nat
double x = add x x

square :: Nat -> Nat
square x = mul x x
