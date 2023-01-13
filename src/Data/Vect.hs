module Data.Vect where

import Prelude hiding (length, (++))
import Data.Kind
import Data.Level
import Data.Nat
import Data.Fin


data Vect :: Nat -> Type -> Type where
    Nil  :: Vect Z a
    Cons :: a -> Vect len a -> Vect (S len) a


instance Functor (Vect len) where
    fmap f Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)


instance Foldable (Vect len) where
    foldr f z Nil         = z
    foldr f z (Cons x xs) = f x (foldr f z xs)


instance Traversable (Vect len) where
    traverse f Nil         = pure Nil
    traverse f (Cons x xs) = do
        x' <- f x
        xs' <- traverse f xs
        pure $ Cons x' xs'


(++) :: Vect len a -> Vect len' a -> Vect (Add len len') a
Nil       ++ ys = ys
Cons x xs ++ ys = Cons x (xs ++ ys)


length :: Vect len a -> SNat len
length Nil         = SZ
length (Cons x xs) = SS (length xs)


index :: Fin len -> Vect len a -> a
index FZ     (Cons x xs) = x
index (FS i) (Cons x xs) = index i xs


level :: Level len -> Vect len a -> a
level (Level l) xs =
    let i = complement (length xs) l
    in index i xs


findLevel :: Eq a => a -> Vect len a -> Maybe (Level len)
findLevel e Nil         = Nothing
findLevel e (Cons x xs)
    | e == x    = Just (Level (limit $ length xs))
    | otherwise = weakenLevel <$> findLevel e xs