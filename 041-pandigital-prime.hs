import Data.Char (digitToInt)
import Data.List (sort)


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


largestPandigitalPrime :: Int -> Int
largestPandigitalPrime n = if isPrime n && isPandigital n then n else largestPandigitalPrime (n - 2)
    where
        isPandigital :: Int -> Bool
        isPandigital n =
            let nrDigits = floor $ (logBase 10 $ fromIntegral n) + 1
                digits = map digitToInt $ show n
            in sort digits == [1..nrDigits]


main :: IO()
main = do
    let maxValue = 7654321
    print $ largestPandigitalPrime maxValue
