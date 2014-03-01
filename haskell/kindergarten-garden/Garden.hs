module Garden (garden, defaultGarden, lookupPlants, Plant(..)) where

data Plant = Radishes | Clover | Grass | Violets
             deriving (Show, Eq, Ord, Enum)

garden :: [String] -> String -> [Plant]
garden = undefined

defaultGarden :: String -> [Plant]
defaultGarden = undefined

lookupPlants :: String -> [Plant] -> [Plant]
lookupPlants = undefined


