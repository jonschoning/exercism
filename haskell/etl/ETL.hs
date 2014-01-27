module ETL (transform) where

import qualified Data.Map.Strict as M
import Data.Char (toLower)

transform :: M.Map Int [String] -> M.Map String Int
transform = M.fromList . concatMap swap . M.toList
  where swap (v, xs) = map (\s -> (map toLower s, v)) xs

