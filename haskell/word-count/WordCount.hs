module WordCount
  ( wordCount
  ) where

import Data.Char
import qualified Data.Map.Lazy as M

wordCount :: String -> M.Map String Int
wordCount = foldr addToMap M.empty . tokenize
  where
    addToMap s = M.insertWith (+) s 1
    tokenize =
      words .
      map
        (\c ->
           if isLetter c || isNumber c
             then toLower c
             else ' ')
