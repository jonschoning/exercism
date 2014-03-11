module Luhn (checkDigit, addends, checksum, isValid, create) where

import Data.Char (digitToInt)

checkDigit :: Integer -> Integer
checkDigit = (`mod` 10)

addends :: Integer -> [Integer]
addends i = map snd . scanr go (True, i `mod` 10) $ (init.show) i
  where go xs (double, _) = let x = (toInteger.digitToInt) xs 
                                norm a = if a >= 10 then a - 9 else a
                            in if double then (False, norm (x*2)) else (True, x)

checksum :: Integer -> Integer
checksum = checkDigit . sum . addends

isValid :: Integer -> Bool
isValid = (0 ==) . checksum

create :: Integer -> Integer
create n = (+ base) . (`mod` 10) . (10-) . checksum $ base
  where base = n * 10


