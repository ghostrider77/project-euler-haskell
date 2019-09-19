import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M


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


calcPrimeFactorization :: Int -> CanonicalForm
calcPrimeFactorization n = go n primes M.empty
    where
        primes = calcPrimes n
        go :: Int -> [Int] -> CanonicalForm -> CanonicalForm
        go _ [] factorization = factorization
        go k (p : pss) factorization =
            if k == 1 then factorization
            else
                let (remainder, alpha) = findLargestExponent k p
                    updatedFactorization = if alpha == 0 then factorization else M.insert p alpha factorization
                in go remainder pss updatedFactorization


getNrDivisors :: Int -> Int
getNrDivisors n =
    let factorization = calcPrimeFactorization n
    in M.foldl (\acc alpha -> acc * (alpha + 1)) 1 factorization


calcTriangleNumbers :: Int -> Int
calcTriangleNumbers limit = go 1
    where
        go :: Int -> Int
        go n =
            let triangleNumber = n * (n + 1) `div` 2
                nrDivisors = getNrDivisors triangleNumber
            in if (nrDivisors > limit) then triangleNumber else go (n + 1)


main :: IO()
main = do
    let limit = 500
    print $ calcTriangleNumbers limit
