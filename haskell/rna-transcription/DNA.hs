module DNA (toRNA) where

toRNA :: String -> String
toRNA = map toRNAChar
  where 
    toRNAChar 'T' = 'U'
    toRNAChar z = z
          

