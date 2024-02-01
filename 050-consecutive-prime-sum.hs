import Data.IntSet (fromDistinctAscList)
import qualified Data.IntSet as S


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


sumOfConsecutivePrimes :: Int -> Int
sumOfConsecutivePrimes limit = fst $ go (0, 0) primes
    where
        primes = takeWhile (<= limit) $ filter isPrime [2..]
        primeSet = fromDistinctAscList primes
        go :: (Int, Int) -> [Int] -> (Int, Int)
        go acc [] = acc
        go acc @ (primeSum, longestLength) primelist =
            let cumsums = takeWhile (<=limit) $ scanl1 (+) primelist
                (s, l) = head $ dropWhile (\(n, _) -> S.notMember n primeSet) $ reverse $ zip cumsums [1..]
                acc' = if l > longestLength then (s, l) else acc
            in go acc' (tail primelist)


main :: IO()
main = do
    let limit = 1000000
    print $ sumOfConsecutivePrimes limit
