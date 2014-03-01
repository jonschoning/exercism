module CryptoSquare (normalizePlaintext,
                     squareSize,
                     plaintextSegments,
                     ciphertext,
                     normalizeCiphertext) where

import Data.Char (toLower, isAlphaNum)
import Data.List (find, transpose)
import Data.List.Split (chunksOf)

normalizePlaintext :: String -> String
normalizePlaintext = map toLower.filter isAlphaNum

squareSize :: String -> Int
squareSize = ceiling . sqrt . fromIntegral . length

plaintextSegments :: String -> [String]
plaintextSegments = (chunksOf =<< squareSize) . normalizePlaintext
-- plaintextSegments = join (chunksOf.squareSize) . normalizePlaintext
-- plaintextSegments = (chunksOf <$> squareSize <*> id) . normalizePlaintext

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . chunksOf 5 . ciphertext
