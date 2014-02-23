module Binary (toDecimal) where

import Data.List (foldl')

toDecimal :: String -> Integer
toDecimal = reduce . map digit
  where 
    reduce = foldl' (\b a -> a + 2*b) 0
    digit '1' = 1
    digit _ = 0
    
