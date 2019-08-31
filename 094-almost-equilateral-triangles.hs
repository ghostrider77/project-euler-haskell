calcPerimeterSumOfTriangle :: Int -> Int
calcPerimeterSumOfTriangle limit = go 0 (7, 4)
    where
        go :: Int -> (Int, Int) -> Int
        go acc (x, y) =
            let (side1, side2) = ((2*x + 1) `div` 3, (2*x - 1) `div` 3)
                (perimeter1, perimeter2) = (3*side1 + 1, 3*side2 - 1)
            in  if perimeter1 >= limit && perimeter2 >= limit then acc
                else
                    let acc' = if (2*x + 1) `mod` 3 == 0 && perimeter1 <= limit then acc + perimeter1 else acc
                        acc'' = if (2*x - 1) `mod` 3 == 0 && perimeter2 <= limit then acc + perimeter2 else acc'
                    in go acc'' (2*x + 3*y, 2*y + x)


main :: IO()
main = do
    let perimeterLimit = 1000000000
    print $ calcPerimeterSumOfTriangle perimeterLimit
