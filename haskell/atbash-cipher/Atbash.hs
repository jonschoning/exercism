{-# LANGUAGE OverloadedStrings #-}

module Atbash (encode) where

import Data.List.Split (chunksOf)
import Data.Char (ord,chr,toLower,isAlphaNum,isDigit)

encode :: String -> String
encode = unwords
         . chunksOf 5
         . map update
         . filter isAlphaNum
  where update c 
          | isDigit c = c 
          | otherwise = chr $ 219 - ord(toLower c)

