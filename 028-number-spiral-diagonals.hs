sumOfSpiralDiagonals :: Int -> Int
sumOfSpiralDiagonals limit = go 1 2 2 1 [1, 0, 0, 0]
    where
        go :: Int -> Int -> Int -> Int -> [Int] -> Int
        go s levelDiff rowDiff level cornerVector
            | level >= limit = s
            | otherwise =
                let first = head cornerVector + levelDiff
                    updatedVector = map (\k -> first + k * rowDiff) [0..3]
                in go (s + sum updatedVector) (levelDiff + 8) (rowDiff + 2) (level + 2) updatedVector


main :: IO()
main = do
    let limit = 1001
    print $ sumOfSpiralDiagonals limit
