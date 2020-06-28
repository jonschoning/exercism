module School
  ( School
  , add
  , empty
  , grade
  , sorted
  ) where

import Control.Arrow (second)
import Data.List (sort)
import qualified Data.Map.Strict as M

type School = M.Map Int [String]

add :: Int -> String -> School -> School
add g n = M.insertWith (++) g [n]

empty :: School
empty = M.empty :: School

grade :: Int -> School -> [String]
grade = M.findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = sortNames . M.toAscList
  where
    sortNames = map $ second sort
