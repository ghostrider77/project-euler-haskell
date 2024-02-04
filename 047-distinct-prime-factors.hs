isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


hasKFactors :: Int -> Int -> [Int] -> Bool
hasKFactors n k = go n 0
    where
        go :: Int -> Int -> [Int] -> Bool
        go _ _ [] = error "No more prime numbers"
        go m nrFactors (p : ps)
            | p > m = nrFactors >= k
            | otherwise =
                let (nrFactors', m') = if m `mod` p == 0 then (nrFactors + 1, m `div` p) else (nrFactors, m)
                in (nrFactors' >= k) || go m' nrFactors' ps


distinctPrimeFactors :: Int -> Int
distinctPrimeFactors size = go 0 2
    where
        primes = filter isPrime [2..]
        go :: Int -> Int -> Int
        go currentSize n
            | not (hasKFactors n size primes) = go 0 (n + 1)
            | otherwise =
                let nextSize = currentSize + 1
                in if nextSize == size then n - size + 1 else go nextSize (n + 1)


main :: IO()
main = do
    let size = 4
    print $ distinctPrimeFactors size
