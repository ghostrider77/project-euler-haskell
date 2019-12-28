isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


totientMaximum :: Int -> Int
totientMaximum limit =
    let primes = filter isPrime [1..]
        products = scanl1 (*) primes
    in last $ takeWhile (< limit) products


main :: IO()
main = do
    let limit = 1000000
    print $ totientMaximum limit
