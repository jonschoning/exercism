module Queens
  ( boardString
  , canAttack
  ) where

import Data.List (intersperse)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M

type Coord = (Integer, Integer)

boardString :: Maybe Coord -> Maybe Coord -> String
boardString w b = pretty . fmap getBoardChar $ allCoords
  where
    allCoords = [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
    getBoardChar coord = M.findWithDefault 'O' coord board
    pretty = unlines . map (intersperse ' ') . chunksOf 8
    board =
      let b0 = M.empty
          b1 = maybe b0 (\p -> M.insert p 'W' b0) w
          b2 = maybe b1 (\p -> M.insert p 'B' b1) b
      in b2

canAttack :: Coord -> Coord -> Bool
canAttack (x1, y1) (x2, y2) = isRowCol || isDiag
  where
    isRowCol = x1 == x2 || y1 == y2
    isDiag = abs (x1 - x2) == abs (y1 - y2)
