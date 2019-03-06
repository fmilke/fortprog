module Utils (mapNth) where

-- map like function, but only applies to the nth element
mapNth :: (a -> a) -> [a] -> Int -> [a]
mapNth fn (a:as) 1 = (fn a) : as
mapNth _  []     _ = error ("Access out of bound index")
mapNth fn (a:as) n = a : (mapNth fn as (n - 1))