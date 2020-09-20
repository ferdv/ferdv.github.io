{----------------------------------
 - CS4400 Code Examples from 9/15 -
 ----------------------------------}

-- Note that I write comments, purpose statements, section headings using the
-- Haddock syntax. See https://www.haskell.org/haddock if you want to know more.

-- * Type signatures

-- ;; name : Type

x :: Integer
x = 14

c :: Char
c = 'c'

name :: String
name = "Zaphod"

list :: [Integer]
list = [1, 2, 3]

list1 :: [[Bool]]
list1 = [[True], [False, True]]

{-
;; area-of-square : Number -> Number
;; Determines the area of a square with the given side.
(define (area-of-square x) (* x x))
-}

-- Determines...
areaOfSquare :: Integer -> Integer
areaOfSquare x = x * x

-- ;; takes2 : Number Number -> String

takes2 :: Integer -> Integer -> String
takes2 x y = undefined

type People = [String]

-- type String = [Char]


data Color = Red | Green | Blue

data NumberOrName = Number Integer
                  | Name String

exampleNumber = Number 42
exampleName = Name "ZAphod"

data MyIntegerList = Nil
                   | Cons Integer MyIntegerList

exampleMyList = Cons 1 (Cons 2 Nil)

myLength :: MyIntegerList -> Integer
myLength Nil = 0
myLength (Cons n l) = 1 + myLength l

colorToString :: Color -> String
colorToString color =
  case color of
       Red -> "red"
       Green -> "green"
       Blue -> "blue"

isName :: NumberOrName -> Bool
isName (Name _) = True
isName _ = False

-- 1 : 2 : []

-- <SAE> ::= <Integer>
--         | <SAE> + <SAE>
--         | <SAE> - <SAE>
--         | ...

data SAE = Num Integer
         | Add SAE SAE
         | Sub SAE SAE
         | Mul SAE SAE
         | Div SAE SAE


