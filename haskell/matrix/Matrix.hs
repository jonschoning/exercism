{-# LANGUAGE BangPatterns #-}

module Matrix ( Matrix, row, column, rows, cols, shape, transpose
              , reshape, flatten, fromString, fromList) where

import qualified Data.Vector as V

data Matrix a = Matrix (V.Vector (V.Vector a))
                deriving (Show, Eq)

type Index = Integer
type Shape = (Index, Index)

rows :: Matrix a -> Index
rows = undefined

cols :: Matrix a -> Index
cols = undefined

row :: Index -> Matrix a -> V.Vector a
row = undefined

column :: Index -> Matrix a -> V.Vector a
column = undefined

shape :: Matrix a -> Shape 
shape = undefined

transpose :: Matrix a -> Matrix a
transpose  = undefined

reshape :: Shape -> Matrix a -> Matrix a
reshape = undefined

flatten :: Matrix a -> V.Vector a
flatten = undefined

fromString :: String -> Matrix a
fromString = undefined

fromList :: [[a]] -> Matrix a
fromList = undefined
