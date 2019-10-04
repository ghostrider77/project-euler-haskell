import Data.Char (digitToInt)
import Data.IntSet (IntSet, fromAscList)
import qualified Data.IntSet as S


calcPrimes :: Int -> IntSet
calcPrimes upperLimit = fromAscList $ filter isPrime $ 2 : [3,5..upperLimit]
    where
        isPrime :: Int -> Bool
        isPrime n
            | n == 2 = True
            | n == 1 || even n = False
            | otherwise =
                let limit = floor $ sqrt $ fromIntegral n
                in all (\k -> n `mod` k /= 0) [3,5..limit]


generateAllShifts :: Int -> [Int]
generateAllShifts p =
    let s = show p
        ds = s ++ s
        size = length s
    in map read $ [take size $ drop k ds | k <- [0..(size-1)]]


getCircularPrimes :: Int -> Int
getCircularPrimes limit = S.foldl checkIfCircular 0 primes
    where
        primes = calcPrimes limit
        checkIfCircular :: Int -> Int -> Int
        checkIfCircular acc p =
            let circularShifts = generateAllShifts p
                allPrime = all (\k -> S.member k primes) circularShifts
            in if allPrime then acc + 1 else acc


main :: IO()
main = do
    let limit = 1000000
    print $ getCircularPrimes limit
