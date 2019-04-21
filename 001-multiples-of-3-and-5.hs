calcSumUpTo :: Int -> Int
calcSumUpTo limit = sum $ takeWhile (< limit) $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..]


main :: IO()
main = do
    let limit = 1000
    let result = calcSumUpTo limit
    print result
