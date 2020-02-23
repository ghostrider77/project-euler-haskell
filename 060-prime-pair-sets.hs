import Data.List (sort)
import Control.Monad (guard)


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


isCompatible :: Int -> Int -> Bool
isCompatible p q =
    let ps = show p
        qs = show q
    in (isPrime $ read (ps ++ qs)) && (isPrime $ read (qs ++ ps))


nextPrime :: Int -> Int
nextPrime n = head $ filter isPrime [(n+1)..]


calcCompatiblePrimes :: [Int] -> Int -> [[Int]]
calcCompatiblePrimes primes p =
    let pCompatiblePrimes = filter (isCompatible p) primes
        compatibleSets = do
            q1 <- pCompatiblePrimes
            q2 <- filter (\q -> q /= q1 && isCompatible q q1) pCompatiblePrimes
            q3 <- filter (\q -> q /= q1 && q /= q2) pCompatiblePrimes
            guard (isCompatible q3 q1 && isCompatible q3 q2)
            q4 <- filter (\q -> q /= q1 && q /= q2 && q /= q3) pCompatiblePrimes
            guard (isCompatible q4 q1 && isCompatible q4 q2 && isCompatible q4 q3)
            return [q1, q2, q3, q4, p]
    in compatibleSets


lowestPrimePairSetSum :: Int -> Int
lowestPrimePairSetSum n =
    let initialPrimes = take (n - 1) $ filter isPrime [1..]
        r = last initialPrimes
        go :: [Int] -> Int -> Int
        go firstPrimes p =
            case calcCompatiblePrimes firstPrimes p of [] -> go (p : firstPrimes) (nextPrime p)
                                                       cs -> head $ sort $ map sum cs
    in go initialPrimes (nextPrime r)


main :: IO()
main = do
    let n = 5
    print $ lowestPrimePairSetSum n
