module Series
  ( digits
  , slices
  , largestProduct
  ) where

import Data.Char (digitToInt)
import Data.List (maximum, product, tails)

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n = filter ((n ==) . length) . map (take n) . tails . digits

largestProduct :: Int -> String -> Int
largestProduct i s = safeMax $ map product $ slices i s
  where
    safeMax [] = 1
    safeMax xs = maximum xs
