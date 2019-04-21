calcSumSquareDifference :: Int -> Int
calcSumSquareDifference n =
    let (a, b) = foldr (\k (sq, s) -> (sq + k ^ 2, s + k)) (0, 0) [1..n]
    in b ^ 2 - a


main :: IO()
main = do
    let limit = 100
    let result = calcSumSquareDifference limit
    print result
