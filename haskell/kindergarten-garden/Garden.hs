module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import Data.Maybe (fromJust)
import Data.List.Split
import Data.List (sort)
import Data.Char (ord)
import Data.Tuple (swap)
import Control.Arrow (second)

data Plant = Radishes | Clover | Grass | Violets | None
             deriving (Show, Eq, Ord)

instance Enum Plant where
  fromEnum = fromJust . flip lookup plantEnum 
  toEnum = fromJust . flip lookup (map swap plantEnum)
plantEnum = map (second ord) [(Radishes, 'R'), (Clover, 'C'), (Grass, 'G'), (Violets, 'V'), (None, '.')]

garden :: [String] -> String -> [Plant]
garden children g = concat $ foldl filler [[]] pairWithStartIndexes
  where childStartIndexes = map childIndex (sort children)
        gs = defaultGardens g
        pairWithStartIndexes = zip childStartIndexes (chunksOf 2 (zip (head gs) (last gs)))
        filler acc next 
          | acc == [[]] = map padNone [fst gSquare, snd gSquare]
          | otherwise = [head acc ++ padNone (fst gSquare), last acc ++ padNone (snd gSquare)]
          where gSquare = unzip $ snd next
                padNoneLength = (fst next) - length (head acc)
                padNone = ((replicate padNoneLength None) ++)

defaultGarden :: String -> [Plant]
defaultGarden = concat . defaultGardens

defaultGardens :: String -> [[Plant]]
defaultGardens = map (map (toEnum . ord)) . lines

lookupPlants :: String -> [Plant] -> [Plant]
lookupPlants child plants = take 2 . drop (childIndex child) =<< makeLists 
  where makeLists = chunksOf =<< flip quot 2 . length $ plants

childIndex :: String -> Int
childIndex = (2 *) . subtract (ord 'A') . ord . head
