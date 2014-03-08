module Series (digits, slices, largestProduct) where

import Data.Char (digitToInt)
import Data.List (tails, product, maximum)

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n = filter ((n==).length) . map (take n) . tails . digits

largestProduct :: Int -> String -> Int
largestProduct i s = maximum $ map product $ slices i s
