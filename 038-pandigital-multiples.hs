import Data.Char (digitToInt)
import Data.List (sort)


largestConcatenatedPandigitalProduct :: Int -> Int
largestConcatenatedPandigitalProduct n0 = foldl processNext n0 [1..9876]
    where
        allDigits = [1..9]
        largestPossibleNumber = 987654321
        processNext :: Int -> Int -> Int
        processNext acc n = let k = generated n in if isPandigital k && k > acc then k else acc
        generated :: Int -> Int
        generated n = go n 2
            where
                go :: Int -> Int -> Int
                go number k
                    | number > largestPossibleNumber = 0
                    | otherwise =
                        let multiplied = n * k
                            p = length $ show multiplied
                            m = number * (10 ^ p) + multiplied
                        in if m > 123456789 && m <= largestPossibleNumber then m else go m (k + 1)
        isPandigital :: Int -> Bool
        isPandigital n = let digits = map digitToInt $ show n in sort digits == allDigits


main :: IO()
main = do
    let startValue = 918273645
    print $ largestConcatenatedPandigitalProduct startValue
