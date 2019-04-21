isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


sumOfPrimes :: Int -> Int
sumOfPrimes limit = sum $ takeWhile (< limit) $ filter isPrime [2..]


main :: IO()
main = do
    let limit = 2000000
    let result = sumOfPrimes limit
    print result
