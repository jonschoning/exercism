module Meetup
  ( Weekday(..)
  , Schedule(..)
  , meetupDay
  ) where

import Control.Applicative
import Control.Monad (liftM2)
import Data.List (filter)
import Data.Time.Calendar
  ( Day
  , fromGregorian
  , gregorianMonthLength
  , toGregorian
  )
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Teenth
  | Last
  deriving (Enum, Eq)

data Weekday
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Show)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month
  | schedule == Last = last $ fromFirstDayOfMonth isWeekday
  | schedule == Teenth =
    head $ fromFirstDayOfMonth $ (&&) <$> isWeekday <*> isDayTeenth
  | otherwise = head $ drop (fromEnum schedule) $ fromFirstDayOfMonth isWeekday
  where
    toMonthDay (_, _, x) = x
    isNumTeenth = flip elem [13, 14, 15, 16, 17, 18, 19]
    isDayTeenth = isNumTeenth . toMonthDay . toGregorian
    isWeekday = (show weekday ==) . formatTime defaultTimeLocale "%A"
    fromFirstDayOfMonth f =
      filter f $
      take (gregorianMonthLength year month) $
      iterate succ $ fromGregorian year month 1
