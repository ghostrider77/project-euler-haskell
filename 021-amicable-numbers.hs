import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Data.Vector (Vector(..), (!), generate)

type CanonicalForm = Map Int Int


calcPrimes :: Int -> [Int]
calcPrimes upperLimit = filter isPrime $ 2 : [3,5..upperLimit]
    where
        isPrime :: Int -> Bool
        isPrime n
            | n == 2 = True
            | n == 1 || even n = False
            | otherwise =
                let limit = floor $ sqrt $ fromIntegral n
                in all (\k -> n `mod` k /= 0) [3,5..limit]


findLargestExponent :: Int -> Int -> (Int, Int)
findLargestExponent n p = go n 0
    where
        go :: Int -> Int -> (Int, Int)
        go k alpha = if k `mod` p /= 0 then (k, alpha) else go (k `div` p) (alpha + 1)


calcPrimeFactorization :: Int -> [Int] -> CanonicalForm
calcPrimeFactorization n primes = go n primes M.empty
    where
        go :: Int -> [Int] -> CanonicalForm -> CanonicalForm
        go _ [] factorization = factorization
        go k (p : pss) factorization =
            if k == 1 then factorization
            else
                let (remainder, alpha) = findLargestExponent k p
                    updatedFactorization = if alpha == 0 then factorization else M.insert p alpha factorization
                in go remainder pss updatedFactorization


calcSumOfProperDivisors :: Int -> Vector Int
calcSumOfProperDivisors n = generate n (\k -> sumOfProperDivisors (k + 1))
    where
        primes = calcPrimes n
        sumOfProperDivisors :: Int -> Int
        sumOfProperDivisors k =
            let factorization = calcPrimeFactorization k primes
                divisorSum = M.foldlWithKey
                    (\acc p alpha -> acc * ((p ^ (alpha + 1) - 1) `div` (p - 1))) 1 factorization
            in divisorSum - k


sumOfAmicableNumbers :: Int -> Int
sumOfAmicableNumbers n =
    let aliquotSum = calcSumOfProperDivisors n
    in sum [a + b | b <- [3..n], a <- [2..b-1], (aliquotSum ! (a - 1)) == b && (aliquotSum ! (b - 1)) == a]


main :: IO()
main = do
    let limit = 10000
    print $ sumOfAmicableNumbers limit
