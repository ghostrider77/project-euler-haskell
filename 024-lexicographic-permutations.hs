import Data.List (intercalate)


factorial :: Int -> Int
factorial n = product [1..n]


nthPermutation :: Int -> [Int]
nthPermutation n = go [] allDigits (length allDigits) (n - 1)
    where
        allDigits = [0..9]
        go :: [Int] -> [Int] -> Int -> Int -> [Int]
        go perm _ 0 _ = reverse perm
        go perm digits ix limit =
            let fact = factorial (ix - 1)
                m = limit `div` fact
                r = limit `mod` fact
                digit = digits !! m
                shortenedDigits = fst $ unzip $ filter (\(_, k) -> k /= m) $ zip digits [0..]
            in go (digit : perm) shortenedDigits (ix - 1) r


main :: IO()
main = do
    let n = 1000000
    let result = nthPermutation n
    putStrLn $ intercalate "" $ map show result
