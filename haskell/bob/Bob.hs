module Bob where

import Data.Char

responseFor :: String -> String
responseFor a
  | isEmpty = "Fine. Be that way!"
  | isCaps = "Woah, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    isEmpty = all isSpace a
    isCaps = all isUpper onlyAlpha && not (null onlyAlpha)
    isQuestion = last a == '?'
    onlyAlpha = filter isAlpha a
