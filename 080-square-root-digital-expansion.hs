import qualified Data.IntSet as S


sumOfNthDigits :: Int -> Int -> Int
sumOfNthDigits limit digitLimit = foldl (\acc n -> acc + sumOfFirstNDigitsSquareRoot n) 0 [1..limit]
    where
        squares = S.fromAscList $ takeWhile (<= limit) $ map (^2) [1..]
        sumOfFirstNDigitsSquareRoot :: Int -> Int
        sumOfFirstNDigitsSquareRoot n
            | S.member n squares = 0
            | otherwise = let (s, _, _) = foldl advanceToNextDigit (0, toInteger n, 0) [1..digitLimit] in s
        reversedDigits = [9,8..0]
        advanceToNextDigit :: (Int, Integer, Integer) -> Int -> (Int, Integer, Integer)
        advanceToNextDigit (s, c, p) _ =
            let (x, y) = head $ dropWhile (\(_, y) -> y > c) $ map (\d -> (d, d*(20*p + d))) reversedDigits
            in (s + fromIntegral x, 100*(c - y), 10*p + x)


main :: IO()
main = do
    let digitLimit = 100
    let limit = 100
    print $ sumOfNthDigits limit digitLimit
