import Data.Array (Array(..), listArray, (!))


data Direction = Horizontal | Vertical | DiagonalDown | DiagonalUp


convertToIntList :: String -> [Int]
convertToIntList = map read . words


convertToMatrix :: [[Int]] -> Int -> Array (Int, Int) Int
convertToMatrix rows n = listArray ((0, 0), (n - 1, n - 1)) $ concat rows


productOfElems :: (Int, Int) -> Direction -> Array (Int, Int) Int -> Int
productOfElems (i, j) direction matrix =
    let indicesOfAdjacentElems = case direction of
            Horizontal -> [(i, j), (i, j + 1), (i, j + 2), (i, j + 3)]
            Vertical -> [(i, j), (i + 1, j), (i + 2, j), (i + 3, j)]
            DiagonalDown -> [(i, j), (i + 1, j + 1), (i + 2, j + 2), (i + 3, j + 3)]
            DiagonalUp -> [(i, j), (i - 1, j + 1), (i - 2, j + 2), (i - 3, j + 3)]
    in product $ map (matrix !) indicesOfAdjacentElems


maxFourAdjacentProduct :: Array (Int, Int) Int -> Int -> Int
maxFourAdjacentProduct matrix size =
    let rowMax = foldl (\acc indices -> let p = productOfElems indices Horizontal matrix in max p acc) 0
                     [(ix, jy) | jy <- [0..size-4], ix <- [0..size-1]]
        colMax = foldl (\acc indices -> let p = productOfElems indices Vertical matrix in max p acc) 0
                     [(ix, jy) | jy <- [0..size-1], ix <- [0..size-4]]
        leftMax = foldl (\acc indices -> let p = productOfElems indices DiagonalDown matrix in max p acc) 0
                      [(ix, jy) | jy <- [0..size-4], ix <- [0..size-4]]
        rightMax = foldl (\acc indices -> let p = productOfElems indices DiagonalUp matrix in max p acc) 0
                       [(ix, jy) | jy <- [0..size-4], ix <- [3..size-1]]
    in maximum [rowMax, colMax, leftMax, rightMax]


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0011.dat")
    let rows = map convertToIntList content
    let size = length rows
    let matrix = convertToMatrix rows size
    print $ maxFourAdjacentProduct matrix size
