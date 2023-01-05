module Data.List.All where

import Data.Kind


data All :: (a -> Type) -> [a] -> Type where
    Nil :: All p '[]
    Cons :: p x -> All p xs  -> All p (x ': xs)
