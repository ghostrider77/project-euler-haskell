nrLargerNominators :: Int -> Int
nrLargerNominators limit = go 2 0 (1, 3) (1, 2)
    where
        (a, b) = (1, 2)
        go :: Int -> Int -> (Integer, Integer) -> (Integer, Integer) -> Int
        go k count (n0, n1) (d0, d1)
            | k >= limit = count
            | otherwise =
                let n2 = b*n1 + a*n0
                    d2 = b*d1 + a*d0
                    nrDigitsN = length $ show n2
                    nrDigitsD = length $ show d2
                    count' = if nrDigitsN > nrDigitsD then count + 1 else count
                in go (k + 1) count' (n1, n2) (d1, d2)


main :: IO()
main = do
    let limit = 1000
    print $ nrLargerNominators limit
