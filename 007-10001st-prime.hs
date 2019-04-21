isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


getNthPrime :: Int -> Int
getNthPrime n = primes !! (n - 1)
    where primes = filter isPrime [2..]


main :: IO()
main = do
    let n = 10001
    let result = getNthPrime n
    print result
