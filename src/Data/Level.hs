module Data.Level where

import Data.Fin
import Data.Nat


newtype Level (vars :: Nat) = Level (Fin vars)


weakenLevel :: Level vars -> Level (S vars)
weakenLevel (Level l) = Level (relax l)