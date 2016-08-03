{-Validating credit card numbers-}


--Get every second number a list--
getEverySecondDouble :: [Int] -> [Int]
getEverySecondDouble [] = []
getEverySecondDouble (x:[]) = []
getEverySecondDouble xs = [(xs !! 0)*2] ++ getEverySecondDouble (drop 2 xs)

--Get every non double--
getEveryNonDouble :: [Int] -> [Int]
getEveryNonDouble [] = []
getEveryNonDouble (x:[]) = []
getEveryNonDouble xs = [(xs !! 1)] ++ getEveryNonDouble (drop 2 xs)

isCardValid :: [Int] -> Bool
isCardValid xs = if (sum (getEverySecondDouble xs)) + (sum (getEveryNonDouble xs)) `mod` 10 == 0
        then True
        else False