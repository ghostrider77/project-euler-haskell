import Data.Array (Array(..), listArray, (!), range)


convertToIntList :: String -> [Int]
convertToIntList = map read . words


convertToMatrix :: [[Int]] -> Int -> Array (Int, Int) Int
convertToMatrix rows n = listArray ((0, 0), (n - 1, n - 1)) $ concat $ map padwithZeros rows
    where
        padwithZeros :: [Int] -> [Int]
        padwithZeros list = let len = length list in list ++ replicate (n - len) 0


calcMaxPathSum :: Array (Int, Int) Int -> Int -> Int
calcMaxPathSum matrix n = maximum $ map (\colIx -> maxPathSumMatrix ! (n - 1, colIx)) [0..(n - 1)]
    where
        maxPathSum :: Int -> Int -> Int
        maxPathSum 0 0 = matrix ! (0, 0)
        maxPathSum i 0 = maxPathSumMatrix ! (i - 1, 0) + matrix ! (i, 0)
        maxPathSum i j
            | j > i = 0
            | otherwise =
                let number = matrix ! (i, j)
                in max (maxPathSumMatrix ! (i - 1, j) + number) (maxPathSumMatrix ! (i - 1, j - 1) + number)
        bounds = ((0, 0), (n - 1, n - 1))
        maxPathSumMatrix = listArray bounds [maxPathSum ix jy | (ix, jy) <- range bounds]


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0018.dat")
    let rows = map convertToIntList content
    let size = length $ last rows
    let matrix = convertToMatrix rows size
    print $ calcMaxPathSum matrix size
