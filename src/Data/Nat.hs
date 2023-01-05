module Data.Nat where

import Data.Kind


data Nat
    = Z
    | S Nat


data SNat :: Nat -> Type where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)


type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add Z     m = m
    Add (S n) m = S (Add n m)