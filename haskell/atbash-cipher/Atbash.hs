module Atbash
  ( encode
  ) where

import Data.Char (chr, isAlphaNum, isDigit, ord, toLower)
import Data.List.Split (chunksOf)

encode :: String -> String
encode = unwords . chunksOf 5 . map update . filter isAlphaNum
  where
    update c
      | isDigit c = c
      | otherwise = chr $ ord 'z' + ord 'a' - ord (toLower c)
