data Convergents = Convergents { x :: Integer, y :: Integer}


calcFundamentalSolution :: Integer -> Convergents
calcFundamentalSolution n = go difference (m1, d1, a1) c0 c1
    where
        a0 = floor $ sqrt $ fromIntegral n
        advanceTriples :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
        advanceTriples (m, d, a) =
            let m' = d*a - m
                d' = (n - m'^2) `div` d
                a' = (a0 + m') `div` d'
            in (m', d', a')
        (m1, d1, a1) = advanceTriples (0, 1, a0)
        c0 = Convergents a0 1
        c1 @ (Convergents x1 y1) = Convergents (a0*a1 + 1) a1
        difference = x1^2 - n*y1^2
        go :: Integer -> (Integer, Integer, Integer) -> Convergents -> Convergents -> Convergents
        go difference (m', d', a') (Convergents x y) c'@(Convergents x' y')
            | difference == 1 = c'
            | otherwise =
                let (m'', d'', a'') = advanceTriples (m', d', a')
                    c'' @ (Convergents x'' y'') = Convergents (a''*x' + x) (a''*y' + y)
                    s = x''^2 - n*y''^2
                in go s (m'', d'', a'') c' c''


maximalFundamentalSolution :: Integer -> Integer
maximalFundamentalSolution limit =
    let squares = takeWhile (<= limit) $ map (^2) [1..]
        processNextElem :: (Integer, Integer) -> Integer -> (Integer, Integer)
        processNextElem acc @ (xMax, nMax) n
            | n `elem` squares = acc
            | otherwise = let Convergents x _ = calcFundamentalSolution n in if x > xMax then (x, n) else acc
    in snd $ foldl processNextElem (0, 0) [1..limit]



main :: IO()
main = do
    let limit = 1000
    print $ maximalFundamentalSolution limit
