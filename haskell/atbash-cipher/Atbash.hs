{-# LANGUAGE OverloadedStrings #-}

module Atbash (encode) where

import Data.List (intersperse)
import Data.Char (ord,chr,toLower,isAlphaNum,isDigit)
import Data.Text (unpack,pack,chunksOf)
import qualified Data.Text as T (concat)

encode :: String -> String
encode = unpack 
         . T.concat
         . intersperse " "
         . chunksOf 5
         . pack
         . map update
         . filter isAlphaNum
  where update c 
          | isDigit c = c 
          | otherwise = chr $ 219 - ord(toLower c)

