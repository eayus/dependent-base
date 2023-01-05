module Data.List.Util where


type family Append (xs :: [a]) (ys :: [a]) :: [a] where
    Append '[] ys = ys
    Append (x : xs) ys = x : Append xs ys