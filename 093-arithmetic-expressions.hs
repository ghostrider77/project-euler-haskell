import Data.List (intercalate, permutations, subsequences)
import Data.Set (Set(..))
import qualified Data.Set as S


operators :: [Float -> Float -> Float]
operators = [(+), (-), (*), (/)]


combinations :: Int -> [a] -> [[a]]
combinations k = filter ((== k) . length) . subsequences


isReal :: Float -> Bool
isReal x = not (isNaN x || isInfinite x)


isInteger :: Float -> Bool
isInteger x = abs (x - fromIntegral (round x)) <= 1e-8


firstMissing :: [Int] -> Int
firstMissing list = length $ takeWhile (\(x, k) -> x == k) $ zip list [1..]


calcLongestConsecutiveLength :: [Int] -> Int
calcLongestConsecutiveLength digits =
    let possibleValues = performAllOperations $ map fromIntegral digits
        positiveIntegers = map round $ S.toAscList $ S.filter (\x -> isInteger x && x > 0) possibleValues
    in firstMissing positiveIntegers


performAllOperations :: [Float] -> Set Float
performAllOperations digits =
    let calculatedValues = do
        [a, b, c, d] <- permutations digits
        op1 <- operators
        op2 <- operators
        op3 <- operators
        [op2 (op1 a b) (op3 c d),
         op3 (op2 (op1 a b) c) d,
         op1 a (op2 b (op3 c d)),
         op3 (op1 a (op2 b c)) d,
         op1 a (op3 (op2 b c) d)]
    in S.fromList $ filter isReal calculatedValues


findMostExpressiveFourDigits :: [Int] -> [Int]
findMostExpressiveFourDigits allDigits = go (0, []) fourDigits
    where
        fourDigits = combinations 4 allDigits
        go :: (Int, [Int]) -> [[Int]] -> [Int]
        go (_, maxDigits) [] = maxDigits
        go (maxSequence, maxDigits) (digits : rest) =
            let result = calcLongestConsecutiveLength digits
                acc = if (result > maxSequence) then (result, digits) else (maxSequence, maxDigits)
            in go acc rest


main :: IO()
main = do
    let digits = [1..9]
    let result = findMostExpressiveFourDigits digits
    putStrLn $ intercalate "" $ map show result
