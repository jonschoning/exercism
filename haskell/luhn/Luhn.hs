module Luhn (checkDigit, addends, checksum, isValid, create) where

import Data.Char (digitToInt)

checkDigit :: Integer -> Integer
checkDigit = (`mod` 10)

addends :: Integer -> [Integer]
addends = reverse
          . zipWith ($) (cycle [id, doubler])
          . map (toInteger . digitToInt)
          . reverse
          . show
  where doubler = let f x = if x >= 10 then x - 9 else x 
                  in f . (*2)
                  
checksum :: Integer -> Integer
checksum = checkDigit . sum . addends

isValid :: Integer -> Bool
isValid = (0 ==) . checksum

create :: Integer -> Integer
create n = (+ n10) . (`mod` 10) . (10-) . checksum $ n10
  where n10 = n * 10


