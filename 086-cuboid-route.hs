nrIntegerShortestPaths :: Int -> Int
nrIntegerShortestPaths a =
    let isPythagorean :: Int -> Int -> Bool
        isPythagorean x y =
            let z = sqrt $ fromIntegral $ x^2 + y^2
            in abs (fromIntegral (round z) - z) < 1e-8
        testTriples :: Int -> Int -> Int
        testTriples acc bc
            | isPythagorean a bc = acc + (min (a + 1) bc - (bc + 1) `div` 2)
            | otherwise = acc
    in foldl testTriples 0 [1..2*a]


smallestDimensionForCuboidRoutes :: Int -> Int
smallestDimensionForCuboidRoutes limit = go 0 1
    where
        go :: Int -> Int -> Int
        go count a
            | count >= limit = a - 1
            | otherwise = go (count + nrIntegerShortestPaths a) (a + 1)


main :: IO()
main = do
    let limit = 1000000
    print $ smallestDimensionForCuboidRoutes limit
