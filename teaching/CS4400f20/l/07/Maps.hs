module Maps where

type Map k v = [(k, v)]

empty :: Map k v
empty = []

get :: Eq k => Map k v -> k -> Maybe v
get ((y, v) : m) x | x == y = Just v
                   | otherwise = get m x
get [] x = Nothing

set :: Map k v -> k -> v -> Map k v 
set m x v = (x, v) : m


