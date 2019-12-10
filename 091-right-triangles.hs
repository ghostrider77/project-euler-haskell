import Data.List (sort)


data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord)


distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
    let dx = x2 - x1
        dy = y2 - y1
    in sqrt $ fromIntegral ((dx * dx) + (dy * dy))


numberOfRightTriangles :: Int -> Int
numberOfRightTriangles limit = foldl (\acc (p, q) -> if isRightTriangle p q then acc + 1 else acc) 0 vertices
    where
        gridpoints = [Point a b | a <- [0..limit], b <- [0..limit], (a, b) /= (0, 0)]
        origin = Point 0 0
        vertices = [(p, q) | p <- gridpoints, q <- gridpoints, p < q]
        isRightTriangle :: Point -> Point -> Bool
        isRightTriangle p q =
            let d1 = distance origin p
                d2 = distance origin q
                d3 = distance p q
                [a, b, c] = sort [d1, d2, d3]
                diff = c ** 2 - (a ** 2 + b ** 2)
            in abs diff <= 1e-8


main :: IO()
main = do
    let limit = 50
    print $ numberOfRightTriangles limit
