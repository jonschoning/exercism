module Robot (Bearing(..), Robot, mkRobot,
              coordinates, simulate,
              bearing, turnRight, turnLeft) where

import Control.Monad (liftM2)
import Data.List (foldl')

data Bearing = North | East | South | West
               deriving (Show, Eq, Enum, Ord, Bounded)

type Coordinate = (Int, Int)

data Robot = MkRobot { 
                bearing :: Bearing,
                coordinates :: Coordinate }

instance Show Robot where
  show r = (show.bearing $ r) 
           ++ " " ++ 
           (show.coordinates $ r)

instance Eq Robot where
  (==) r1 r2 = (show r1) == (show r2)

mkRobot :: Bearing -> Coordinate -> Robot 
mkRobot = MkRobot

simulate :: Robot -> String -> Robot
simulate = foldl' step
  where 
    step r 'R' = r { bearing = turnRight.bearing $ r }
    step r 'L' = r { bearing = turnLeft.bearing $ r }
    step r 'A' = r { coordinates = liftM2 nextCoord bearing coordinates r }
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
