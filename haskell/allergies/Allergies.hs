module Allergies
  ( Allergen(..)
  , isAllergicTo
  , allergies
  ) where

import Data.Bits (testBit)

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Show, Eq, Ord, Enum)

isAllergicTo :: Allergen -> Integer -> Bool
isAllergicTo a = flip testBit $ fromEnum a

allergies :: Integer -> [Allergen]
allergies i = filter (`isAllergicTo` i) $ enumFrom Eggs
