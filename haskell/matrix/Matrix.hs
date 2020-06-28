{-# LANGUAGE BangPatterns #-}

module Matrix
  ( Matrix
  , row
  , column
  , rows
  , cols
  , shape
  , transpose
  , reshape
  , flatten
  , fromString
  , fromList
  ) where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Vector as V

type Matrix a = V.Vector (V.Vector a)

type Index = Int

type Shape = (Index, Index)

rows :: Matrix a -> Index
rows = V.length

cols :: Matrix a -> Index
cols m =
  if V.null m
    then 0
    else V.length (row 0 m)

row :: Index -> Matrix a -> V.Vector a
row = flip (V.!)

column :: Index -> Matrix a -> V.Vector a
column c m = V.generate (rows m) atRow
  where
    atRow r = (m V.! r) V.! c

shape :: Matrix a -> Shape
shape = (rows &&& cols)

transpose :: Matrix a -> Matrix a
transpose m = V.fromList $ map (flip column m) [0 .. cols m - 1]

reshape :: Shape -> Matrix a -> Matrix a
reshape (r, c) = V.ifoldl' go (V.replicate r V.empty) . flatten
  where
    go !accM i x =
      let appendToCol accRowInd accV =
            if accRowInd /= (i `div` (c))
              then accV
              else V.snoc accV x
      in V.imap appendToCol accM

flatten :: Matrix a -> V.Vector a
flatten = join

fromString :: (Read a) => String -> Matrix a
fromString xs = fromList . map parseLine . lines $ xs

parseLine :: (Read a) => String -> [a]
parseLine xs = map read . tokenize $ xs
  where
    tokenize s =
      if (head s == '\"')
        then tokenizeString s
        else words s
    tokenizeString = map T.unpack . tokenizeText . T.pack
    tokenizeText = map wrapDelimiters . filterNotEmpty . splitDelimiters
    splitDelimiters = T.splitOn (T.pack "\"")
    wrapDelimiters = (flip T.snoc) '\"' . T.cons '\"'
    filterNotEmpty = filter (not . T.null . T.dropWhile isSpace)

fromList :: [[a]] -> Matrix a
fromList = V.fromList . map V.fromList
