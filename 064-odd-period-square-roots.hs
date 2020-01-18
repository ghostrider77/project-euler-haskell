import qualified Data.IntSet as S


calcSquareRootExpansion :: Int -> [Int]
calcSquareRootExpansion n = go [] 0 1 a0
    where
        a0 = floor $ sqrt $ fromIntegral n
        go :: [(Int, Int, Int)] -> Int -> Int -> Int -> [Int]
        go acc m d a
            | (m, d, a) `elem` acc = drop 1 $ reverse $ map (\(_, _, a) -> a) acc
            | otherwise =
                let m' = d*a - m
                    d' = (n - m'^2) `div` d
                    a' = (a0 + m') `div` d'
                in go ((m, d, a) : acc) m' d' a'


nrOddPeriodContinuedFractions :: Int -> Int
nrOddPeriodContinuedFractions limit =
    let squares = S.fromAscList $ takeWhile (<= limit) $ map (^2) [1..]
        squareRootExpansionPeriod :: Int -> Int
        squareRootExpansionPeriod n
            | S.member n squares = 0
            | otherwise = length $ calcSquareRootExpansion n
    in foldl (\acc n -> if odd $ squareRootExpansionPeriod n then acc + 1 else acc) 0 [1..limit]


main :: IO()
main = do
    let limit = 10000
    print $ nrOddPeriodContinuedFractions limit
