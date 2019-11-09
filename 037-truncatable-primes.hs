import Data.List (inits, tails)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


isTruncatable :: Int -> IntSet -> Bool
isTruncatable p primeSet = p >= 11 && leftTruncatable && rightTruncatable
    where
        allTruncationsPrime :: (String -> [String]) -> Bool
        allTruncationsPrime func = all (\x -> S.member x primeSet) $ map read (filter (not . null) (func $ show p))
        leftTruncatable = allTruncationsPrime tails
        rightTruncatable = allTruncationsPrime inits


sumOfTruncatablePrimes :: Int -> Int
sumOfTruncatablePrimes limit = go [] 0 S.empty allPrimes
    where
        allPrimes = filter isPrime [2..]
        go :: [Int] -> Int -> IntSet -> [Int] -> Int
        go truncatablePrimes k primeSet unprocessedPrimes
            | k == limit = sum truncatablePrimes
            | otherwise =
                let p : rest = unprocessedPrimes
                    primeSet' = S.insert p primeSet
                in if isTruncatable p primeSet' then go (p : truncatablePrimes) (k + 1) primeSet' rest
                   else go truncatablePrimes k primeSet' rest


main :: IO()
main = do
    let n = 11
    print $ sumOfTruncatablePrimes n
