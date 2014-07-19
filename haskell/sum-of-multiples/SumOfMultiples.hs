module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

sumOfMultiplesDefault :: Integer -> Integer
sumOfMultiplesDefault = sumOfMultiples [3,5]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples ms x = sum $ takeWhile (< x) (multiples ms)

multiples :: [Integer] -> [Integer]
multiples ms = filter isValid [1..]
  where isValid n = any (\a -> rem n a == 0) ms
