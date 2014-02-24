module Scrabble (scoreLetter, scoreWord) where

import Data.Maybe (fromJust)
import Data.Char (toUpper)
import qualified Data.HashMap.Strict as M

letterValues :: M.HashMap Char Integer
letterValues = M.fromList $ expandPairs [(1, "AEIOULNRST"), (2, "DG"), (3, "BCMP"), (4, "FHVWY"), (5, "K"), (8, "JX"), (10, "QZ")]
  where expandPairs = (expandPair =<<)
        expandPair (i, s) = fmap (\c-> (c, i)) s

scoreLetter :: Char -> Integer
scoreLetter = fromJust . flip M.lookup letterValues . toUpper

scoreWord :: String -> Integer
scoreWord = foldr ((+) . scoreLetter) 0
