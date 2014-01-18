module DNA (count, nucleotideCounts) where

import Data.List
import qualified Data.Map.Strict as M

count :: Char -> String -> Int
count c 
  | c `elem` "ACGTU" = length.filter (==c) 
  | otherwise = error $ "invalid nucleotide \'" ++ c : "\'"

nucleotideCounts :: String -> M.Map Char Int
nucleotideCounts = foldl' addToMap emptyMap
  where
    emptyMap = M.fromList [('A', 0), ('T', 0), ('C', 0), ('G', 0)]
    addToMap m c = M.insertWith (+) c 1 m

