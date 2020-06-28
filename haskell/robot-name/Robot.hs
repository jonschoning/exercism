module Robot
  ( robotName
  , mkRobot
  , resetName
  ) where

import Control.Applicative
import Data.IORef
import System.Random

newtype Robot = Robot
  { name :: IORef String
  }

mkRobot :: IO Robot
mkRobot = Robot <$> (generateName >>= newIORef)

robotName :: Robot -> IO String
robotName = readIORef . name

resetName :: Robot -> IO ()
resetName r = generateName >>= writeIORef (name r)

generateName :: IO String
generateName = mapM randomRIO $ replicate 2 ('A', 'Z') ++ replicate 3 ('0', '9')
