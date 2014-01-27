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
secondsToEarthYears s = fromIntegral s / 3.15576e7
