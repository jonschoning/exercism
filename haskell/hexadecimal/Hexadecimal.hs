module Hexadecimal (hexToInt) where

import Data.List (foldl')
import Data.Char (isDigit, digitToInt)
import Control.Monad (liftM2)

hexToInt :: String -> Int
hexToInt h | any (liftM2 (&&) (not.isDigit) (not.(`elem` ['a'..'f']))) h = 0
hexToInt h = foldl' (\b a -> digit a + 16*b) 0 h
  where 
    digit 'a' = 10
    digit 'b' = 11
    digit 'c' = 12
    digit 'd' = 13
    digit 'e' = 14
    digit 'f' = 15
    digit x = digitToInt x
