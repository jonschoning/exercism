module ETL
  ( transform
  ) where

import Data.Char (toLower)
import qualified Data.Map.Strict as M

transform :: M.Map Int [String] -> M.Map String Int
transform = M.fromList . concatMap swap . M.toList
  where
    swap (v, xs) = map (\s -> (map toLower s, v)) xs
