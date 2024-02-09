isPentagonal :: Int -> Bool
isPentagonal k =
    let n = (1 + sqrt (fromIntegral (1 + 24*k))) / 6
    in fromInteger (round n) == n


smallestPentagonalDifference :: Int
smallestPentagonalDifference = go pentagonals 1
    where
        pentagonals = map (\n -> n*(3*n - 1) `div` 2) [1..]
        go :: [Int] -> Int -> Int
        go (p : rest) ix = case innerLoop p (take (ix - 1) pentagonals) of
            Nothing -> go rest (ix + 1)
            Just difference -> difference
        innerLoop :: Int -> [Int] -> Maybe Int
        innerLoop _ [] = Nothing
        innerLoop p (q : qs) =
            let s = p + q
                d = p - q
            in if isPentagonal s && isPentagonal d then Just d else innerLoop p qs


main :: IO()
main = do
    print smallestPentagonalDifference
