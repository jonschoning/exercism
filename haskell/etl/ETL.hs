module ETL (transform) where

import qualified Data.Map.Strict as M
import Data.Char (toLower)

transform :: M.Map Int [String] -> M.Map String Int
transform m = M.fromList $ swapConcat $ M.toList m

swapConcat :: [(Int, [String])] -> [(String, Int)]
swapConcat = concatMap (\p -> map (\s -> (lower s, fst p)) $ snd p)
  where lower = map toLower

