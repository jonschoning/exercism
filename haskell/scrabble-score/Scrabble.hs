module Scrabble (scoreLetter, scoreWord) where

import Data.Maybe (fromJust)
import Data.Char (toUpper)
import qualified Data.Map.Strict as M

letterValues :: M.Map Char Integer
letterValues = M.fromList $ expandPairs [(1, "AEIOULNRST"), (2, "DG"), (3, "BCMP"), (4, "FHVWY"), (5, "K"), (8, "JX"), (10, "QZ")]
  where expandPairs pair = pair >>= expandPair
          where expandPair (i, s) = fmap (\c-> (c, i)) s

scoreLetter :: Char -> Integer
scoreLetter = fromJust . flip M.lookup letterValues . toUpper

scoreWord :: String -> Integer
scoreWord = foldr ((+) . scoreLetter) 0
