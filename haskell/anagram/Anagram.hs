module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram
  where isAnagram a b
          | la == lb  = False
          | otherwise = sort la == sort lb
          where
            la = map toLower a
            lb = map toLower b


