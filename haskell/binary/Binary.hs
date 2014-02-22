module Binary (toDecimal) where

import Control.Applicative ((<$>),(<*>))
import Data.Char (digitToInt)

toDecimal :: String -> Integer
toDecimal s = if not (isValid s) then 0 else convert s
  where 
    isValid = all $ (||) <$> (=='1') <*> (=='0')
    convert = toInteger . foldr (+) 0 . expandBases
    expandBases = (zipWith (\b a -> digitToInt a * 2^b) [0..]) . reverse
    
