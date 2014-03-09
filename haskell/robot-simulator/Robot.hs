module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft) where

import Control.Applicative ((<*>), (<$>))
import Data.List (foldl')
import Control.DeepSeq (NFData, rnf, ($!!))

data Bearing = North | East | South | West
               deriving (Show, Eq, Enum, Ord, Bounded)

type Coordinate = (Int, Int)
type Commands = String

data Robot = Robot { 
                bearing :: Bearing,
                coordinates :: Coordinate }
               deriving (Eq, Show)

instance NFData Robot where
  rnf (Robot bearing coordinate) = bearing `seq` rnf coordinate

mkRobot :: Bearing -> Coordinate -> Robot 
mkRobot = Robot

simulate :: Robot -> Commands -> Robot
simulate = foldl' (step $!!)
  where 
    step r 'R' = r { bearing = turnRight.bearing $ r }
    step r 'L' = r { bearing = turnLeft.bearing $ r }
    step r 'A' = r { coordinates = nextCoord <$> bearing <*> coordinates $ r }
    step r _   = error "unknown command"

nextCoord :: Bearing -> Coordinate -> Coordinate
nextCoord North (x, y) = (x, succ y)
nextCoord South (x, y) = (x, pred y)
nextCoord East (x, y) = (succ x, y)
nextCoord West (x, y) = (pred x, y)

turnRight :: Bearing -> Bearing
turnRight b 
  | b == maxBound = minBound
  | otherwise = succ b
            
turnLeft :: Bearing -> Bearing
turnLeft b
  | b == minBound = maxBound
  | otherwise = pred b
