isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


primeSums :: Int -> [Int] -> Int
primeSums 0 _ = 1
primeSums _ [] = 0
primeSums n (p : rest) = go 0 0
    where
        go :: Int -> Int -> Int
        go acc k =
            let n' = n - k * p
            in if n' < 0 then acc else go (acc + primeSums n' rest) (k + 1)


solvePrimeSummation :: Int -> Int
solvePrimeSummation limit = go 2 []
    where
        go :: Int -> [Int] -> Int
        go n primes =
            let primes' = if isPrime n then n : primes else primes
                nrWays = primeSums n primes'
            in if nrWays > limit then n else go (n + 1) primes'


main :: IO()
main = do
    let limit = 5000
    print $ solvePrimeSummation limit
