module Gigasecond
  ( fromDay
  ) where

import Data.Time.Calendar (Day, addDays)

fromDay :: Day -> Day
fromDay = addDays $ round $ 1000000000 / 60 / 60 / 24
