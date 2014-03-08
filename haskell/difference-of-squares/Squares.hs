module Squares (sumOfSquares, squareOfSums, difference) where

import Control.Applicative ((<$>),(<*>))

sumOfSquares :: Integral a => a -> a 
sumOfSquares n = sum $ fmap (^2) [1..n]

squareOfSums :: Integral a => a -> a
squareOfSums n = (^2) $ sum [1..n]

difference :: Integral a => a -> a
difference = subtract <$> sumOfSquares <*> squareOfSums
