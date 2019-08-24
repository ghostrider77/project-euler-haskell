limit :: Integer
limit = 1000000000000


solveNegativePellEquation :: Integer
solveNegativePellEquation = go 1 1
    where
        go :: Integer -> Integer -> Integer
        go x y =
            if (x + 1) `div` 2 < limit then go (3*x + 4*y) (2*x + 3*y)
            else (y + 1) `div` 2


main :: IO()
main = do
    let blue = solveNegativePellEquation
    print blue
