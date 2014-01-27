module SpaceAge (Planet(..), ageOn) where

import qualified Data.Map.Strict as M

data Planet =
    Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  deriving (Ord, Eq)

periodFromEarth :: M.Map Planet Float
periodFromEarth = M.fromList [(Mercury, 0.2408467),
                              (Venus, 0.61519726),
                              (Mars, 1.8808158),
                              (Earth, 1),
                              (Jupiter, 11.862615),
                              (Saturn, 29.447498),
                              (Uranus, 84.016846),
                              (Neptune, 164.79132)]

ageOn :: Planet -> Int -> Float
ageOn planet = (/ periodFromEarth M.! planet) . toEarthYears 
  where toEarthYears seconds = fromIntegral seconds / 3.15576e7

