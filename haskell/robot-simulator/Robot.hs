module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft) where

data Bearing = North | South | East | West
               deriving (Show, Eq, Enum)

type Coordinate = (Int, Int)

data Robot = Robot

instance Show Robot where
  show r = undefined

instance Eq Robot where
  (==) r1 r2 = undefined

mkRobot :: Bearing -> Coordinate -> Robot 
mkRobot = undefined

coordinates :: Robot -> Coordinate
coordinates = undefined

simulate :: Robot -> String -> Robot
simulate = undefined

bearing :: Robot -> Bearing
bearing = undefined

turnRight :: Bearing -> Bearing
turnRight = undefined

turnLeft :: Bearing -> Bearing
turnLeft = undefined
