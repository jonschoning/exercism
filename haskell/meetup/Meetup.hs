module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.List (filter)

data Schedule = First | Second | Third | Fourth | Teenth | Last
  deriving (Enum, Eq)

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month
  | schedule `elem` enumFromTo First Fourth = head $ 
                                              drop (fromEnum schedule) $ 
                                              filter isWeekday fromFirstDayOfMonth
  | schedule == Last = last $ filter isWeekday $ fromFirstDayOfMonth
  | schedule == Teenth = head $ filter isWeekday $ filter (isTeenth . toMonthDay . toGregorian) fromFirstDayOfMonth
  where
    fromFirstDayOfMonth = take (gregorianMonthLength year month) $
                          iterate succ $ 
                          fromGregorian year month 1
    toMonthDay (_,_,x) = x
    isTeenth = flip elem [13, 14, 15, 16, 17, 18, 19]
    isWeekday d = formatTime defaultTimeLocale "%A" d == (show weekday)
