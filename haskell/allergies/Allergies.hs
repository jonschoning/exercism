module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Map.Strict as M
import Data.Bits ((.&.))
import Control.Monad (liftM)

data Allergen = Eggs
                | Peanuts
                | Shellfish
                | Strawberries 
                | Tomatoes
                | Chocolate
                | Pollen
                | Cats
                deriving (Show, Eq, Ord)

allergens :: Map Allergen Integer
allergens = M.fromList [ (Eggs, 1)
                        ,(Peanuts, 2)
                        ,(Shellfish, 4)
                        ,(Strawberries , 8)
                        ,(Tomatoes, 16)
                        ,(Chocolate, 32)
                        ,(Pollen, 64)
                        ,(Cats, 128)]

isAllergicTo :: Allergen -> Integer -> Bool
isAllergicTo a i = maybe False (liftM (> 0) (.&. i)) $ M.lookup a allergens

allergies :: Integer -> [Allergen]
allergies i = keys $ M.filter (liftM (> 0) (.&. i)) allergens
