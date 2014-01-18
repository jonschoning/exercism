module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance a b = length.filter (==False) $ zipWith (==) a b
