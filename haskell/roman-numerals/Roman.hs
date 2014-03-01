module Roman (numerals) where

import Data.Map.Strict as M
import Data.List (unfoldr)
import Control.Applicative ((<$>))

conversions :: M.Map Int String
conversions = M.fromList [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"), (50,"L"), 
                          (40,"XL"), (10,"X"), (9,"IX"), (5,"V"), (4,"IV"), (1,"I")]

numerals :: Int -> String
numerals = concat . unfoldr convert
  where convert n = (\(val, str) -> (str, n-val)) <$> M.lookupLE n conversions
