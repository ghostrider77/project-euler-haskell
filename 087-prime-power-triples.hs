import qualified Data.IntSet as S


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


numberOfPrimePowerSums :: Int -> Int
numberOfPrimePowerSums limit =
    let primes = filter isPrime [1..]
        [p2, p3, p4] = map (\k -> takeWhile (<= limit) $ map (^k) primes) [2, 3, 4]
    in S.size $ S.fromList [p + q + r | p <- p2, q <- p3, r <- p4, p + q + r < limit]


main :: IO()
main = do
    let limit = 50000000
    print $ numberOfPrimePowerSums limit
