{-Validating credit card numbers-}


{- Exercise 1: We need to first find the digits of a number.

Example: toDigits 1234 == [1,2,3,4]
Example: toDigitsRev 1234 == [4,3,2,1]
Example: toDigits 0 == []
Example: toDigits (-17) == []

-}


toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    |  n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
    |  otherwise = []



{- Exercise 2: Once we have the digits in the proper order, we need to
double every other one. 

Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Example: doubleEveryOther [1,2,3] == [1,4,3]

-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) $ cycle [1,2]

{-Exercise 3: The output of doubleEveryOther has a mix of one-digit and
and two-digit numbers. 

Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22

-}
sumDigits :: [Integer] -> Integer
sumDigits x = sum (map (sum . toDigits) x)


{-Exercise 4: -}
validate :: Integer -> Bool
validate n
    | n > 0 =  (sumDigits (doubleEveryOther (toDigitsRev n))) `mod` 10 == 0
    | otherwise = False

