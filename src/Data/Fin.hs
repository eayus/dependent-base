module Data.Fin where

import Data.Kind
import Data.Nat


data Fin :: Nat -> Type where
    FZ :: Fin (S n)
    FS :: Fin n -> Fin (S n)


deriving instance Eq (Fin vars)


finToNat :: Fin n -> Nat
finToNat FZ     = Z
finToNat (FS n) = S (finToNat n)


relax :: Fin n -> Fin (S n)
relax FZ     = FZ
relax (FS i) = FS (relax i)


limit :: SNat n -> Fin (S n)
limit SZ     = FZ
limit (SS n) = FS (limit n)


complement :: SNat n -> Fin n -> Fin n
complement (SS n) FZ     = limit n
complement (SS n) (FS i) = relax $ complement n i
