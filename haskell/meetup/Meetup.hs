module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (liftM2)
import Data.List (filter)

data Schedule = First | Second | Third | Fourth | Teenth | Last
  deriving (Enum, Eq)

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month
  | schedule == Last   = last $ filter isWeekday fromFirstDayOfMonth
  | schedule == Teenth = head $ filter (liftM2 (&&) isWeekday isDayTeenth) fromFirstDayOfMonth
  | otherwise          = head $ drop (fromEnum schedule) $ filter isWeekday fromFirstDayOfMonth
  where
    toMonthDay (_,_,x) = x
    isTeenth = flip elem [13, 14, 15, 16, 17, 18, 19]
    isDayTeenth = isTeenth . toMonthDay . toGregorian
    isWeekday d = formatTime defaultTimeLocale "%A" d == (show weekday)
    fromFirstDayOfMonth = take (gregorianMonthLength year month) $
                          iterate succ $ 
                          fromGregorian year month 1
