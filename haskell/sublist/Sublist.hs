module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

import Data.List (isInfixOf)

data Sublist =  Equal 
              | Sublist 
              | Superlist 
              | Unequal deriving (Show, Eq)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist x y 
  | (null x) && (null y) = Equal
  | length x == length y && and (zipWith (==) x y) = Equal
  | x `isInfixOf` y = Sublist
  | y `isInfixOf` x = Superlist
  | otherwise = Unequal

