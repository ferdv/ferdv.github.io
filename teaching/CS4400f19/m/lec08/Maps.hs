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

alloc :: (Ord k, Enum k) => Map k v -> v -> (k, Map k v)
alloc [] init = (first, add first init empty)
  where first = toEnum 0
alloc m init = (newAddress, add newAddress init m)
  where newAddress = succ (maximum (keys m))

allocMaybe :: (Ord k, Enum k) => Map k (Maybe v) -> (k, Map k (Maybe v))
allocMaybe m = alloc m Nothing



