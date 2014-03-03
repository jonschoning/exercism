module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.List (sort, elemIndex)
import Data.Char (ord)
import Data.Tuple (swap)
import Control.Arrow (second)

data Plant = Radishes | Clover | Grass | Violets | None
             deriving (Show, Eq, Ord)

data Garden = DefaultGarden [Plant] | NamedGarden [Plant] [String] 

instance Enum Plant where
  fromEnum = fromJust . flip lookup plantEnum 
  toEnum = fromJust . flip lookup (map swap plantEnum)

plantEnum = map (second ord) [(Radishes, 'R'), (Clover, 'C'), (Grass, 'G'), (Violets, 'V'), (None, '.')]

garden :: [String] -> String -> Garden
garden = flip (NamedGarden . concat . defaultPlants)

defaultGarden :: String -> Garden
defaultGarden = DefaultGarden . concat . defaultPlants

defaultPlants :: String -> [[Plant]]
defaultPlants = map (map (toEnum . ord)) . lines

lookupPlants :: String -> Garden -> [Plant]
lookupPlants child (DefaultGarden plants)  = lookupPlants' (defaultChildIndex child) child plants
  where defaultChildIndex = (2 *) . subtract (ord 'A') . ord . head
lookupPlants child (NamedGarden plants names)  = lookupPlants' (namedChildIndex child (sort names)) child plants
  where namedChildIndex n names = (2 *) $ fromJust $ elemIndex n names

lookupPlants' :: Int -> String -> [Plant] -> [Plant]
lookupPlants' startIndex child plants = take 2 . drop startIndex  =<< makeLists plants
  where makeLists = chunksOf =<< flip quot 2 . length
