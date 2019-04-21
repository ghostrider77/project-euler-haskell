primes :: [Int]
primes = [2, 3, 5, 7, 11, 13, 17, 19]


isDivisible :: Int -> Int -> Bool
isDivisible n limit = all (\k -> n `mod` k == 0) [2..limit]


smallestMultiple :: Int
smallestMultiple = go (product primes) 2
    where
        go :: Int -> Int -> Int
        go n k = if isDivisible n 20 then n else go (k * n) (k + 1)


main :: IO()
main = do
    print smallestMultiple
