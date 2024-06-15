import Data.List.Split (splitOn)
import Data.Array (Array, listArray, (!), range)


convertToMatrix :: [String] -> Int -> Array (Int, Int) Int
convertToMatrix rows n =
    let convertToIntList :: String -> [Int]
        convertToIntList = map read . splitOn ","
    in listArray ((0, 0), (n - 1, n - 1)) $ concatMap convertToIntList rows


shortestPath :: Array (Int, Int) Int -> Int -> Int
shortestPath matrix n = minPathSumMatrix ! (n - 1, n - 1)
    where
        minPathSum :: Int -> Int -> Int
        minPathSum 0 0 = matrix ! (0, 0)
        minPathSum 0 j = minPathSumMatrix ! (0, j - 1) + matrix ! (0, j)
        minPathSum i 0 = minPathSumMatrix ! (i - 1, 0) + matrix ! (i, 0)
        minPathSum i j =
            let shortestUp = minPathSumMatrix ! (i - 1, j)
                shortestLeft = minPathSumMatrix ! (i, j - 1)
            in matrix ! (i, j) + min shortestUp shortestLeft
        bounds = ((0, 0), (n - 1, n - 1))
        minPathSumMatrix = listArray bounds [minPathSum ix jy | (ix, jy) <- range bounds]


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0081.dat")
    let n = length content
    let matrix = convertToMatrix content n
    print $ shortestPath matrix n
