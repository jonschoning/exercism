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
squareSize s = fromMaybe 0 $ find (\i -> fromIntegral i >= sqrt (fromIntegral (length s))) [1..] 

plaintextSegments :: String -> [String]
plaintextSegments p = let np = normalizePlaintext p in chunksOf (squareSize np) np

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . chunksOf 5 . ciphertext
