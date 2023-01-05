module Data.Vect.Sub where

import Data.Kind
import Data.Nat
import Data.Vect


data Sub :: Nat -> Nat -> Type where
    Init :: Sub Z Z
    Keep :: Sub n m -> Sub (S n) (S m)
    Drop :: Sub n m -> Sub n (S m)


subVect :: Sub n m -> Vect m a -> Vect n a
subVect Init       Nil         = Nil
subVect (Keep sub) (Cons x xs) = Cons x (subVect sub xs)
subVect (Drop sub) (Cons x xs) = subVect sub xs