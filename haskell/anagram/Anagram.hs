module Anagram
  ( anagramsFor
  ) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram
  where
    isAnagram a b
      | la == lb = False
      | otherwise = sort la == sort lb
      where
        la = map toLower a
        lb = map toLower b
