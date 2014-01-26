module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day)

data Schedule = First | Second | Third | Fourth | Teenth | Last

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

meetupDay :: Schedule -> Weekday -> Int -> Int -> Day
meetupDay schedule weekday year month = undefined
