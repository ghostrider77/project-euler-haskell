import Data.Array (Array, listArray, (!), range)


calcNumberOfSums :: Int -> Int
calcNumberOfSums target = partitionMatrix ! (target - 1, target)
    where
        nrPartitions :: Int -> Int -> Int
        nrPartitions i j
            | i == 1 || j == 1 = 1
            | i == j = 1 + partitionMatrix ! (i - 1, j)
            | i > j = partitionMatrix ! (j, j)
            | otherwise = partitionMatrix ! (i - 1, j) + partitionMatrix ! (i, j - i)
        bounds = ((1, 1), (target, target))
        partitionMatrix = listArray bounds [nrPartitions ix jy | (ix, jy) <- range bounds]


main :: IO()
main = do
    let n = 100
    print $ calcNumberOfSums n
