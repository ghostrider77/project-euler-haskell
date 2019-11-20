import Data.List (sort)
import Data.Char (digitToInt)
import Data.IntSet (fromDistinctAscList)
import qualified Data.IntSet as S


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


isPermutation :: Int -> Int -> Int -> Bool
isPermutation left p right =
    let sortedDigits :: Int -> [Int]
        sortedDigits = sort . map digitToInt . show
        leftDigits = sortedDigits left
        pDigits = sortedDigits p
        rightDigits = sortedDigits right
    in leftDigits == pDigits && pDigits == rightDigits


digitsOfPrimePermutations :: Int
digitsOfPrimePermutations = head $ filter (/= 148748178147) $ S.foldl (\acc p -> go acc p 2) [] primes
    where
        (nMin, nMax) = (1000, 9999)
        primes = fromDistinctAscList $ filter isPrime [nMin..nMax]
        go :: [Int] -> Int -> Int -> [Int]
        go acc p d
            | left < nMin || right > nMax = acc
            | S.notMember left primes || S.notMember right primes = go acc p (d + 2)
            | otherwise = if isPermutation left p right then go (n : acc) p (d + 2) else go acc p (d + 2)
            where
                (left, right) = (p - d, p + d)
                n = (left * 10000 + p) * 10000 + right


main :: IO()
main = do
    print digitsOfPrimePermutations
