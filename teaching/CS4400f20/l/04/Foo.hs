module Foo where

-- * Importing modules

-- import all public names
import SimpleTests

-- import only specific names
import Data.Char (isAlpha, isDigit)

-- import all names qualified with the given qualifier
import qualified Data.Char as C


-- * Using the SimpleTests module

-- |Compute the length of the given list.
length' :: [a] -> Integer
length' [] = 0
length' (_ : xs) = 1 + length' xs

test_length' = do
  test "length' []" (length' []) 0
  test "length' [\"hello\"]" (length' ["hello"]) 1
  test "length' [1, 2]" (length' [1, 2]) 2
  test "length' [1, 2, 3, 4, 5]" (length' [1, 2, 3, 4, 5]) 5

-- |Reverse the given list
rev :: (Show a, Eq a) => [a] -> [a]   -- the Show and Eq constraints are required by the test function
rev [] = []
rev (x : xs) = rev xs ++ [x]

test_rev = do
  test "rev []" (rev []) ([] :: [Integer])  -- this will come up sometimes: you might need to specify the exact type of an empty list
  test "rev [1]" (rev [1]) [1]
  test "rev [1, 2, 3]" (rev [1, 2, 3]) [3, 2, 1]

-- run all the tests in main
main :: IO ()
main = do
  test_length'
  test_rev

