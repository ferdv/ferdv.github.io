{----------------------------------
 - CS4400 code examples from 9/18 -
 ----------------------------------

  The examples are slightly modified and commented.

 -}

-- Note that I write comments, purpose statements, section headings using the
-- Haddock syntax. See https://www.haskell.org/haddock if you want to know more.

-- * SAE interpreter

-- |Represents the abstract syntax of the "Simple Arithmetic Language"
data SAE = Number Integer            -- <SAE> ::= <Integer>
         | Add SAE SAE               --         | <SAE> + <SAE>
         | Sub SAE SAE               --         | <SAE> - <SAE>
         | Mul SAE SAE               --         | <SAE> * <SAE>
         | Div SAE SAE               --         | <SAE> / <SAE>
         deriving (Eq, Show) -- tell Haskell that we might want to compare and print SAE ASTs

-- |Example expression SAEs
sae1, sae2 :: SAE
sae1 = Sub (Add (Number 1) (Number 3)) (Number 3)
sae2 = Div (Number 44) (Add (Mul (Number 2) (Number 5)) (Number 1))

-- |Evaluate the given SAE as an integer.
eval :: SAE -> Integer
eval (Number n) = n
eval (Add e1 e2) = (+) (eval e1) (eval e2)
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

-- "Tests"
test1, test2 :: Bool
test1 = eval sae1 == 1
test2 = eval sae2 == 4

-- * More Types

-- ** Floating-poing numbers

piFloat :: Float
piFloat = 3.1415927

piDouble :: Double
piDouble = 3.141592653589793

-- ** Tuples

--first :: (Integer, String, Integer) -> Integer
--second :: (Integer, String, Integer) -> String
--first (x, _, _) = x

--third (_, _, x) = x

--(12, "Hello", 32.5)

-- ** Polymorphic types
--
-- Note: I use primed names to avoid clashes with Haskell's Prelude

-- |Compute the length of the given list
length' :: [a] -> Integer
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- |Return the given argument.
id' :: a -> a
id' x = x

-- |Compute a list containing only every other element of the given list.
everyOther :: [a] -> [a] 
everyOther [] = []
everyOther [_] = []
everyOther (_ : y : xs) = y : everyOther xs

-- The first two cases could be replaced by a catch-all case 'everyOther _ = []'
-- as the _last_ equation.


-- |A polymorphic list data definition example.
data List a = Empty
            | Cons a (List a)


-- ** Higher-order functions

-- |Convert all the elements in the given list, using the given conversion function.
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map f xs

-- Class constraint:
-- (+) :: Num a => a -> a -> a


