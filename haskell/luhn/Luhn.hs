module Luhn (checkDigit, addends, checksum, isValid, create) where

import Data.Char
import Data.List

checkDigit :: Integer -> Integer
checkDigit = (`mod` 10)

addends :: Integer -> [Integer]
addends xs = zipWith go genCycle xstr
  where 
    xstr = show xs
    genCycle = drop (length xstr `mod` 2) (cycle [0, 1])
    go isDouble xs = let x = (toInteger.digitToInt) xs 
                         fix a = if a >= 10 then a - 9 else a
                     in 
                         if even isDouble then fix (x*2) else x
  

checksum :: Integer -> Integer
checksum = checkDigit . sum . addends

isValid :: Integer -> Bool
isValid = (0 ==) . checksum

create :: Integer -> Integer
create n = (+ base) . (`mod` 10) . (10-) . checksum $ base
  where 
    base = n * 10


