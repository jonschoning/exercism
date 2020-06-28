module LeapYear
  ( isLeapYear
  ) where

isLeapYear :: Int -> Bool
isLeapYear y = mod4 && (not mod100 || mod100 && mod400)
  where
    mod4 = y `mod` 4 == 0
    mod100 = y `mod` 100 == 0
    mod400 = y `mod` 400 == 0
