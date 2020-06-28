module Triangle
  ( TriangleType(..)
  , triangleType
  ) where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illogical
  deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c
  | a == b && b == c = Equilateral
  | a + b <= c || b + c <= a || a + c <= b = Illogical
  | a == b || b == c || a == c = Isosceles
  | otherwise = Scalene
