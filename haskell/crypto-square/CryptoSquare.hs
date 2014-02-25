module CryptoSquare (normalizePlaintext,
                     squareSize,
                     plaintextSegments,
                     ciphertext,
                     normalizeCiphertext) where

import Data.Char (toLower, isAlphaNum)
import Data.List (find, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

normalizePlaintext :: String -> String
normalizePlaintext = map toLower.filter isAlphaNum

squareSize :: String -> Int
squareSize s = fromMaybe 0 $ find (\i -> i*i >= len) [1..] 
  where len = length s

plaintextSegments :: String -> [String]
plaintextSegments p = let np = normalizePlaintext p in chunksOf (squareSize np) np

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . chunksOf 5 . ciphertext
