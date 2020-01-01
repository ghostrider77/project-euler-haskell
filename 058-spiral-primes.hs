isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


calcLevelSizeInSpiral :: Int
calcLevelSizeInSpiral = go 0 1 1 1 2 2
    where
        go :: Int -> Int -> Int -> Int -> Int -> Int -> Int
        go count diagonal level first leveldiff rowdiff =
            let level' = level + 2
                diagonal' = diagonal + 4
                first' = first + leveldiff
                count' = foldl (\acc k -> let p = first' + k*rowdiff in if isPrime p then acc + 1 else acc) count [0..3]
                levelratio = (fromIntegral count') / (fromIntegral diagonal')
            in if levelratio < 0.1 then level' else go count' diagonal' level' first' (leveldiff + 8) (rowdiff + 2)


main :: IO()
main = do
    print $ calcLevelSizeInSpiral
