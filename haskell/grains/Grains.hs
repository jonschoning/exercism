module Grains
  ( square
  , total
  ) where

square :: Integer -> Integer
square s = 2 ^ (s - 1)

total :: Integer
total = 2 * square 64 - 1
