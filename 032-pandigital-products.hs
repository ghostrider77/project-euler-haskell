import Data.Char (digitToInt)
import Data.List (permutations, subsequences)
import Data.IntSet (IntSet, fromList)
import qualified Data.IntSet as S


variations :: Int -> [Int] -> [[Int]]
variations k elems = concatMap permutations $ filter ((== k) . length) $ subsequences elems


pandigitalProduct :: IntSet -> [Int] -> [Int] -> Int -> Maybe Int
pandigitalProduct digitSet digitsOfA digitsOfB expectedLength =
    if expectedLength == length digitsOfC && digitSet == uniqueDigits then Just c else Nothing
    where
        digitsToNumber :: [Int] -> Int
        digitsToNumber = foldl1 (\acc d -> 10*acc + d)
        a = digitsToNumber digitsOfA
        b = digitsToNumber digitsOfB
        c = a * b
        digitsOfC = map digitToInt $ show c
        uniqueDigits = S.unions [fromList digitsOfA, fromList digitsOfB, fromList digitsOfC]


sumOfPandigitalProducts :: [Int] -> Int
sumOfPandigitalProducts digits = S.foldl (+) 0 $ go S.empty fiveDigitSequences
    where
        fiveDigitSequences = variations 5 digits
        digitSet = fromList digits
        go :: IntSet -> [[Int]] -> IntSet
        go acc [] = acc
        go acc (digitSequence : rest) =
            let (x1, x2) = splitAt 1 digitSequence
                (y1, y2) = splitAt 2 digitSequence
                a = pandigitalProduct digitSet x1 x2 4
                b = pandigitalProduct digitSet y1 y2 4
                putToSet :: Maybe Int -> Maybe Int -> IntSet
                putToSet m1 m2 = case (m1, m2) of (Just x, Just y) -> fromList [x, y]
                                                  (Just x, _) -> S.singleton x
                                                  (_, Just y) -> S.singleton y
                                                  (_, _) -> S.empty

            in go (S.union acc (putToSet a b)) rest


main :: IO()
main = do
    let digits = [1..9]
    print $ sumOfPandigitalProducts digits
