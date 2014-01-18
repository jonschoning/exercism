module Beer (sing, verse) where

import Data.List
import Text.Printf (printf)

sing :: Int -> Int -> String
sing from to = foldl' prependVerse "" [to..from]
  where prependVerse s i = verse i ++ '\n' : s

verse :: Int -> String
verse i 
  | i >  2 = nonFinalVerse (show i ++ " bottles") "one" (show (i-1) ++ " bottles")
  | i == 2 = nonFinalVerse           "2 bottles"  "one"               "1 bottle"
  | i == 1 = nonFinalVerse           "1 bottle"   "it"          "no more bottles"
  | otherwise = finalVerse
  where 
    nonFinalVerse a = printf "%s of beer on the wall, %s of beer.\n\
                             \Take %s down and pass it around, %s of beer on the wall.\n" a a
    finalVerse = "No more bottles of beer on the wall, no more bottles of beer.\n\
                 \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
