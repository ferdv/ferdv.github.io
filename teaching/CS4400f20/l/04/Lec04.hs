{- 
Module      : Lec04
Description : In-class examples from 09/22/2020 9:50am
Copyright   : (c) Ferd, 2020
Maintainer  : f.vesely@northeastern.edu
-}


-- * JSON to BNF to Haskell datatype

{-

  A JSON element is one of:
  - a number (integer)
  - a string
  - a boolean
  - a `null`
  - an array of 0 or more JSON elements (enclosed in [ ])
  - an object, containing 0 or more tag-element pairs (enclosed in { })

Example:
  { "class" : "CS4400",
    "instructor" : "Ferd",
    "students" : 48,
    "days" : [ "Tuesday", "Friday" ]
  }

-}

{- BNF 
-

  <JSON> ::= <Integer>
           | <String>
           | <Boolean>
           | null
           | [ <NonEmptyList>? ]  -- optional
           | { }
           | { <NonEmptyTaggedList> }

  <NonEmptyList> ::= <JSON>
                   | <JSON> , <NonEmptyList>

  <NonEmptyTaggedList> ::= <String> : <JSON>
                         | <String> : <JSON> , <NonEmptyTaggedList>

  <Digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

  Ways to specify iterated elements:

  <Integer> ::= <Digit>
              | <Digit> <Integer>

  or

  <Integer> ::= <Digit> <Digit>*

  or

  <Integer> ::= <Digit>+
  
-}

-- |Represents JSON data 
data Json = Number Integer
          | Str String
          | Boolean Bool
          | Null
          | Array [Json]
          | Object [(String, Json)]
          deriving (Show, Read, Eq, Ord)

-- data JsonObject = JsonObject String Json

emptyJsonArray = Array []
emptyJsonObject = Object []

-- |representation of the above JSON example string
ex_classInfo =
  Object [ ("class", Str "CS4400")
         , ("instructor", Str "Ferd")
         , ("students", Number 48)
         , ("days", Array [ Str "Tuesday", Str "Friday" ])
         ]



-- * Type classes

-- |Represents shapes
data Shape = Circle Float
           | Square Float
           deriving (Show, Eq, Ord, Read)

-- Q: Does Ord make sense here?


-- Class constraing restrict a polymorphic type to members of the class

-- |Check whether the given element is present in the given list.
find :: Eq a => a -> [a] -> Bool
find x [] = False
find x (y : ys) | x == y = True
                | otherwise = find x ys

-- * Enum type class
data Four = One | Two | Three | Four 
          deriving (Show, Enum)

ex_listOfFour = [One .. Three]  -- = [One, Two, Three]

ex_listOfOddInts = [1,3..100]


-- * Higher-order functions

-- We wrote map in the previous lecture
-- map :: (a -> b) -> [a] -> [b]

-- Example: design a function for filtering lists based on a predicate.

-- |Returns a list with only the elements satisfying the given predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs) | p x = x : filter' p xs
                   | otherwise = filter' p xs

-- map captures the following pattern of recursive list functions
-- f [] = []
-- f (x : xs) = ... x ... : f xs

-- folds are more general and capture a more general list recursion pattern:
-- f v [] = v
-- f v (x : xs) = something (... x ...) (f v xs)

-- right-assiciative fold:
-- foldRight f v [x0, x1, x2, ..., xn] = 
--   f x0 (f x1 (f x2 (... (f xn v))))

-- |Combine the elements of the given list using the given function, with the 
-- |given initial value
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight _ v [] = v
foldRight f v (x : xs) = f x (foldRight f v (xs))

-- * Let and where


{- Let bindings are mutually recursive! This will run forever. Why?

let x = y * 2
    y = x + 2
in x * y
-}

-- |Double all integers in the given list
doubleAll :: [Integer] -> [Integer]
doubleAll l = -- let .. in .. is an expression and can be used wherever an expression might appear
  let double x = x + x 
  in map double l

-- |Double all integers in the given list
doubleAll' :: [Integer] -> [Integer]
doubleAll' l = map double l
  where double x = 2 * x  -- where is its own syntactic construct and needs to be adjacent to a definition


-- 'where' is useful, e.g., when several guards repeat the same expression
-- f x | xsq == 2 =  -- error: xsq is not visible here!
-- f x | xsq > 0 =
--     | xsq < 0 =
--     | xsq == 0 =
--   where xsq = x * x

-- * Lambdas
-- in ISL w/ lambda (or Scheme/Racket):
-- (map (lambda (x) (+ x x)) '(1 2 3 4))

-- in Haskell

doubleAll'' :: [Integer] -> [Integer]
doubleAll'' l = map (\x -> x * 2) l

-- multiple arguments
sumList :: [Integer] -> Integer
sumList l = foldr (\x y -> x + y) 0 l

-- note: (\x y -> x + y) can be also expressed as (+)

-- * Sections

-- (* 2) = \x -> x * 2
-- (2 *) = \x -> 2 * x

-- (`div` 2) = \x -> div x 2



