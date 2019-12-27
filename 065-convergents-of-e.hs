import Data.Char (digitToInt)


sumOfNumeratorDigits :: Int -> Int
sumOfNumeratorDigits n = go 1 (2, 3) (1, 1)
    where
        a = 1
        go :: Int -> (Integer, Integer) -> (Integer, Integer) -> Int
        go k (n0, n1) (d0, d1)
            | k >= n = sum $ map digitToInt $ show n0
            | otherwise =
                let b = toInteger $ if k `mod` 3 == 1 then 2*((k `div` 3) + 1) else 1
                    n2 = b*n1 + a*n0
                    d2 = b*d1 + a*d0
                in go (k + 1) (n1, n2) (d1, d2)


main :: IO()
main = do
    let n = 100
    print $ sumOfNumeratorDigits n
