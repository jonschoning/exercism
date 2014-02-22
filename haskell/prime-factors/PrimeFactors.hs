module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = reverse . loop 2 []
  where loop cur acc n 
          | n == 1 = acc
          | n `mod` cur == 0 = loop 2 (cur:acc) (quot n cur) 
          | otherwise = loop (cur+1) acc n


