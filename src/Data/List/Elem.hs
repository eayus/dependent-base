module Data.List.Elem where

import Data.Kind


data Elem :: a -> [a] -> Type where
    Here  :: Elem e (e : xs)
    There :: Elem e xs -> Elem e (x : xs)