module Data.Nat where

import Data.Kind


data Nat
    = Z
    | S Nat
    deriving Eq


data SNat :: Nat -> Type where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)


type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add Z     m = m
    Add (S n) m = S (Add n m)


natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S n) = 1 + natToInt n
