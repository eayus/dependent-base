module Data.List.Length where

import Data.Nat
import Data.Kind


data Length :: [a] -> Nat -> Type where
    LZ :: Length '[] Z
    LS :: Length xs n -> Length (x : xs) (S n)