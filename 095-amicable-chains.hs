import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M
import Data.Vector (Vector(..), (!), generate)

type CanonicalForm = Map Int Int


calcPrimes :: Int -> [Int]
calcPrimes upperLimit =
    let isPrime :: Int -> Bool
        isPrime n
            | n == 2 = True
            | n == 1 || even n = False
            | otherwise =
                let limit = floor $ sqrt $ fromIntegral n
                in all (\k -> n `mod` k /= 0) [3,5..limit]
    in filter isPrime $ [1..upperLimit]


findLargestExponent :: Int -> Int -> (Int, Int)
findLargestExponent n p =
    let go :: Int -> Int -> (Int, Int)
        go k alpha = if k `mod` p /= 0 then (k, alpha) else go (k `div` p) (alpha + 1)
    in go n 0


calcPrimeFactorization :: Int -> [Int] -> CanonicalForm
calcPrimeFactorization n primes =
    let go :: Int -> [Int] -> CanonicalForm -> CanonicalForm
        go _ [] factorization = factorization
        go k (p : pss) factorization
            | k == 1 = factorization
            | otherwise =
                let (remainder, alpha) = findLargestExponent k p
                    factorization' = if alpha == 0 then factorization else M.insert p alpha factorization
                in go remainder pss factorization'
    in go n primes M.empty


calcSumOfProperDivisors :: Int -> Vector Int
calcSumOfProperDivisors n = generate n (\k -> sumOfProperDivisors (k + 1))
    where
        primes = calcPrimes n
        sumOfProperDivisors :: Int -> Int
        sumOfProperDivisors k =
            let factorization = calcPrimeFactorization k primes
                divisorSum = M.foldlWithKey (\acc p alpha ->
                    acc * ((p ^ (alpha + 1) - 1) `div` (p - 1))) 1 factorization
            in divisorSum - k


longestAmicableChain :: Int -> Int
longestAmicableChain limit =
    let properDivisors = calcSumOfProperDivisors limit
        processNext :: (Int, Int) -> Int -> (Int, Int)
        processNext acc @ (_, maxChainLength) n =
            let size = calcChainLength n in if size > maxChainLength then (n, size) else acc
        calcChainLength :: Int -> Int
        calcChainLength n =
            let go :: Int -> [Int] -> Int
                go number chainMembers =
                    let number' = properDivisors ! (number - 1)
                    in if number' == 1 || number' == number || number' < n || number' >= limit then 0
                       else if number' == n then (length chainMembers) + 1
                       else if number' `elem` chainMembers then 0
                       else go number' (number' : chainMembers)
            in go n []
    in fst $ foldl processNext (1, 1) [2..limit]


main :: IO()
main = do
    let limit = 1000000
    print $ longestAmicableChain limit
