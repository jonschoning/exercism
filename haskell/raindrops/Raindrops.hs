module Raindrops (convert) where

import Data.Monoid (mempty)

rules = [(3, "Pling"), (5, "Plang"), (7,"Plong")]

convert :: Integer -> String
convert n = if result == mempty then show n else result 
  where 
    tryRule r s = if n `rem` r == 0 then s else mempty
    result = uncurry tryRule =<< rules
