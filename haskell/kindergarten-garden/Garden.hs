module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.List (sort, elemIndex)
import Data.Char (ord)

data Plant = Radishes | Clover | Grass | Violets | None
             deriving (Show, Eq)

type Plants = [[Plant]]

data Garden = DefaultGarden Plants | NamedGarden Plants [String] 

readPlant :: Char -> Plant
readPlant 'R' = Radishes
readPlant 'C' = Clover 
readPlant 'G' = Grass 
readPlant 'V' = Violets
readPlant _  = None

garden :: [String] -> String -> Garden
garden = flip (NamedGarden . defaultPlants)

defaultGarden :: String -> Garden
defaultGarden = DefaultGarden . defaultPlants

defaultPlants :: String -> Plants
defaultPlants = map (map readPlant) . lines

lookupPlants :: String -> Garden -> [Plant]
lookupPlants child (DefaultGarden plants)  = lookupPlants' (defaultChildIndex child) child plants
  where defaultChildIndex = (2 *) . subtract (ord 'A') . ord . head
lookupPlants child (NamedGarden plants names)  = lookupPlants' (namedChildIndex child (sort names)) child plants
  where namedChildIndex n names = (2 *) $ fromJust $ elemIndex n names

lookupPlants' :: Int -> String -> Plants -> [Plant]
lookupPlants' startIndex child plants = take 2 . drop startIndex  =<< plants
