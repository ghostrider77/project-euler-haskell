import Data.IntSet (IntSet)
import qualified Data.IntSet as S


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


conjectureViolatingNumber :: Int
conjectureViolatingNumber = go (S.singleton 2) 3
    where
        go :: IntSet -> Int -> Int
        go primes n
            | isPrime n = go (S.insert n primes) (n + 2)
            | condition primes n = n
            | otherwise = go primes (n + 2)
        condition :: IntSet -> Int -> Bool
        condition primes n = all (\p -> S.notMember p primes) $ takeWhile (> 1) $ map (\k -> n - 2 * k ^ 2) [1..]


main :: IO()
main = do
    print conjectureViolatingNumber
