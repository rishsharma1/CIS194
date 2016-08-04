{-Validating credit card numbers-}


{- Exercise 1: We need to first find the digits of a number.-}



toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    |  n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
    |  otherwise = []