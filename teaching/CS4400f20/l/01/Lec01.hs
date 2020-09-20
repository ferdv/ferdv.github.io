-- if True then "Yes" else "No"

-- one-line comment

{- multiline
   comment
-}

-- x = 4

-- func application : f x y


add3 x = x + 4

-- add3 5

fact 0 = 1
fact n = n * fact (n - 1)

fact' n = if n > 0 then n * fact (n - 1) else 1

fact'' n | n <= 0 = 1
fact'' n = n * fact (n - 1)

-- [] -- empty list
-- 1 : l -- "cons"
-- [1, 2, 3] = 1 : (2 : (3 : []))

length' [] = 0
length' (x : xs) = 1 + length' xs


head (x : xs) = x

tail (x : xs) = xs

data Tree = Empty
          | Node Tree Int Tree
          deriving Show


