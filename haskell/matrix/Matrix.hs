{-# LANGUAGE BangPatterns #-}

module Matrix ( Matrix, row, column, rows, cols, shape, transpose
              , reshape, flatten, fromString, fromList) where

import qualified Data.Vector as V
import Control.Monad (join)
import Control.Arrow ((&&&))
import Data.Char (isSpace)
import qualified Data.Text as T

data Matrix a = Matrix { q :: Shape, v :: V.Vector a }
                deriving (Show, Eq)
type Index = Int
type Shape = (Index, Index)

rows :: Matrix a -> Index
rows = fst . q
-- rows = V.length

cols :: Matrix a -> Index
cols = snd . q

row :: Index -> Matrix a -> V.Vector a
row i (Matrix (r, c) v) = V.slice start len v
  where start = i * c
        len = c

column :: Index -> Matrix a -> V.Vector a
column i (Matrix (r, c) v) = undefined

shape :: Matrix a -> Shape 
shape = q

transpose :: Matrix a -> Matrix a
transpose (Matrix s v) = Matrix (snd s, fst s) v

reshape :: Shape -> Matrix a -> Matrix a
reshape s' (Matrix s v) = Matrix s' v

flatten :: Matrix a -> V.Vector a
flatten = v

fromString :: (Read a) => String -> Matrix a
fromString xs = fromList . map parseLine . lines $ xs

parseLine :: (Read a) => String -> [a]
parseLine xs = map read . tokenize $ xs
  where tokenize s = if (head s == '\"') then tokenizeString s else words s
        tokenizeString = map T.unpack . tokenizeText . T.pack
        tokenizeText = map wrapDelimiters . filterNotEmpty . splitDelimiters
        splitDelimiters = T.splitOn (T.pack "\"")
        wrapDelimiters = (flip T.snoc) '\"' . T.cons '\"'
        filterNotEmpty = filter (not . T.null . T.dropWhile isSpace)  

fromList :: [[a]] -> Matrix a
fromList l = let v = V.fromList . map V.fromList $ l
             in Matrix (V.length v, maybe 0 V.length (v V.!? 0)) (join v)
