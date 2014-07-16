module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

import Data.List (isInfixOf)

data Sublist =  Equal 
              | Sublist 
              | Superlist 
              | Unequal deriving (Show, Eq)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist xs ys 
  | isEqual True xs ys = Equal
  | xs `isInfixOf` ys = Sublist
  | ys `isInfixOf` xs = Superlist
  | otherwise = Unequal
  where
    isEqual False _      _      = False
    isEqual True  []     []     = True
    isEqual _     (a:as) []     = False
    isEqual _     []     (b:bs) = False
    isEqual _     (a:as) (b:bs) = isEqual (a == b) as bs

