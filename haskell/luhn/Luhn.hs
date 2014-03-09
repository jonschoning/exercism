module Luhn (checkDigit, addends, checksum, isValid, create) where

import Data.Char
import Data.List

checkDigit :: Integer -> Integer
checkDigit = (`mod` 10)

addends :: Integer -> [Integer]
addends = digits
  where 
    digits = foldr f (fromIntegral.digitToInt) . show
    f i x = if even i then p (x*2) else x
    p a = if a >= 10 then a - 9 else a
  

checksum :: Integer -> Integer
checksum = undefined

isValid :: Integer -> Bool
isValid = undefined

create :: Integer -> Integer
create = undefined

