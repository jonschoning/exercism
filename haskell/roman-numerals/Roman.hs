module Roman (numerals) where

numerals :: Int -> String
numerals i = numl i ""
  where numl n s
          | n == 0 = s
          | n >= 1000 = numl (n-1000) (s++"M")
          | n >= 900  = numl  (n-900) (s++"CM")
          | n >= 500  = numl  (n-500) (s++"D")
          | n >= 400  = numl  (n-400) (s++"CD")
          | n >= 100  = numl  (n-100) (s++"C")
          | n >= 90   = numl   (n-90) (s++"XC")
          | n >= 50   = numl   (n-50) (s++"L")
          | n >= 40   = numl   (n-40) (s++"XL")
          | n >= 10   = numl   (n-10) (s++"X")
          | n >= 9    = numl    (n-9) (s++"IX")
          | n >= 5    = numl    (n-5) (s++"V")
          | n >= 4    = numl    (n-4) (s++"IV")
          | n <= 3    = numl    (n-1) (s++"I")
