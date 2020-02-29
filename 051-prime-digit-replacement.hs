import Data.List (subsequences, sort)
import Data.Set (Set)
import qualified Data.Set as S


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


replaceDigits :: String -> [Int] -> [String]
replaceDigits primeString mask = map replaceMaskWithSingleDigit chars
    where
        chars = ['0'..'9']
        replaceMaskWithSingleDigit :: Char -> String
        replaceMaskWithSingleDigit c = map (\(d, ix) -> if ix `elem` mask then c else d) $ zip primeString [0..]


smallestPrimeOfFamily :: [Int] -> Int -> Int -> Maybe Int
smallestPrimeOfFamily primes nrDigits familySize = go primes
    where
        primeStringSet = S.map show $ S.fromAscList primes
        digitMasks = filter (not . null) $ subsequences [0..(nrDigits-1)]
        go :: [Int] -> Maybe Int
        go [] = Nothing
        go (p : rest) =
            let primeString = show p
                replacedPrimeDigitStrings =
                    map (\mask -> filter (\d -> S.member d primeStringSet) $ replaceDigits primeString mask) digitMasks
                primeFamilies = filter ((== familySize) . length) replacedPrimeDigitStrings
            in if null primeFamilies then go rest else Just $ head $ sort $ map read $ concat primeFamilies


calcSmallestPrimeOfASameDigitReplacementFamily :: Int -> Int
calcSmallestPrimeOfASameDigitReplacementFamily size = go 1
    where
        go :: Int -> Int
        go n =
            let d = n - 1
                primes = filter isPrime [10^d..10^(d+1)]
            in case smallestPrimeOfFamily primes d size of Nothing -> go (n + 1)
                                                           Just p -> p


main :: IO()
main = do
    let familySize = 8
    print $ calcSmallestPrimeOfASameDigitReplacementFamily familySize
