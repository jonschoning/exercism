module Phone (areaCode, number, prettyPrint) where

import Data.Char
import Text.Printf (printf)

areaCode :: String -> String
areaCode = take 3

number :: String -> String
number n = if not isValid
           then "0000000000"
           else n'
  where 
    isValid = tenPattern n' || elevenOnePattern n'
    n' = let z = filter isNumber n in
         if elevenOnePattern z then drop 1 z else z

prettyPrint :: String -> String
prettyPrint n = printf "(%s) %s-%s" an bn cn
  where n' = number n
        an = areaCode n'
        bn = take 3 . drop 3 $ n'
        cn = drop 6 n'

tenPattern :: String -> Bool
tenPattern n = 10 == length n

elevenOnePattern :: String -> Bool
elevenOnePattern n = length n == 11 && head n == '1'
