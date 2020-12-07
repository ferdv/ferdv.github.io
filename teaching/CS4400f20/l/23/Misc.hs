module Misc where

import Data.Char (isDigit)
import Data.List (dropWhileEnd)

import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

type Variable = String


makeFresh :: Variable -> [Variable] -> Variable
makeFresh x vars = x'
  where
    stripSuffix = dropWhileEnd isDigit
    varStream = x : map (\n -> stripSuffix x ++ show n) [0..] 
    x' = head $ dropWhile (`elem` vars) varStream


gensym' :: String -> String
gensym' s = unsafePerformIO $ do
  u <- newUnique
  return $ s ++ "#" ++ show (hashUnique u)

gensym :: String -> String
gensym = gensym' . baseName
  where
    baseName :: String -> String
    baseName = takeWhile (/= '#')


