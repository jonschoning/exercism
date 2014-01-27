module SpaceAge (Planet(..), ageOn) where

data Planet = 
    Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

ageOn :: Planet -> Int -> Float
ageOn Earth = secondsToEarthYears 
ageOn _ = undefined

secondsToEarthYears :: Int -> Float
secondsToEarthYears s = 8765.81 * 60 * 60 * (fromIntegral s)
