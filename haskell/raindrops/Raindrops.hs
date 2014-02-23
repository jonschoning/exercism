module Raindrops (convert) where

import Data.Monoid (mempty)

rules :: [(Integer, String)]
rules = [(3, "Pling"), (5, "Plang"), (7,"Plong")]

convert :: Integer -> String
convert n = if result == mempty then show n else result 
  where 
    factors = primeFactors n
    tryRule e s = if e `elem` factors then s else mempty
    result = uncurry tryRule =<< rules

primeFactors :: Integer -> [Integer]
primeFactors = reverse . loop 2 []
  where loop cur acc n 
          | n == 1 = acc
          | n `mod` cur == 0 = loop 2 (cur:acc) (quot n cur) 
          | otherwise = loop (cur+1) acc n


