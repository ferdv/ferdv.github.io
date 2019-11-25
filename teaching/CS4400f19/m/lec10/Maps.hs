module Maps where

type Map k v = [(k, v)]

empty :: Map k v
empty = []

add :: k -> v -> Map k v -> Map k v
add k v m = (k, v) : m

get :: Eq k => k -> Map k v -> Maybe v
get = lookup 

keys :: Map k v -> [k]
keys [] = []
keys ((k, _) : m) = k : keys m


