module Sublist (Sublist(Equal, Sublist, Superlist, Unequal), sublist) where

data Sublist =  Equal 
              | Sublist 
              | Superlist 
              | Unequal deriving (Show, Eq)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist = undefined

