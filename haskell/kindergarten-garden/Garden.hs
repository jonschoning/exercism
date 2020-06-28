module Garden
  ( garden
  , defaultGarden
  , lookupPlants
  , Plant(..)
  ) where

import Data.Char (ord)
import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

data Plant
  = Radishes
  | Clover
  | Grass
  | Violets
  | None
  deriving (Show, Eq)

type Plants = [[Plant]]

data Garden
  = DefaultGarden Plants
  | NamedGarden Plants
                [String]

readPlant :: Char -> Plant
readPlant 'R' = Radishes
readPlant 'C' = Clover
readPlant 'G' = Grass
readPlant 'V' = Violets
readPlant _ = None

garden :: [String] -> String -> Garden
garden = flip (NamedGarden . defaultPlants)

defaultGarden :: String -> Garden
defaultGarden = DefaultGarden . defaultPlants

defaultPlants :: String -> Plants
defaultPlants = map (map readPlant) . lines

lookupPlants :: String -> Garden -> [Plant]
lookupPlants child (DefaultGarden plants) =
  take 2 . drop defaultChildIndex =<< plants
  where
    defaultChildIndex = (2 *) . subtract (ord 'A') . ord . head $ child
lookupPlants child (NamedGarden plants names) =
  take 2 . drop namedChildIndex =<< plants
  where
    namedChildIndex = (2 *) $ fromJust $ elemIndex child (sort names)
