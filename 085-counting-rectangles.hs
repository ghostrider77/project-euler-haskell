import Data.Vector (generate, (!))


findMinArea :: Int -> Int
findMinArea limit = n * k
    where
        (n, k) = snd $ foldl processN (maxBound, (0, 0)) [1..nLimit]
        nLimit = 2000
        nOver2 = generate (nLimit + 1) (\k -> k*(k+1) `div` 2)
        processN :: (Int, (Int, Int)) -> Int -> (Int, (Int, Int))
        processN acc n =
            foldl (\innerAcc @ (minDist, (nMin, kMin)) k ->
                let p = (nOver2 ! n) * (nOver2 ! k)
                    diff = abs (p - limit)
                in if diff < minDist then (diff, (n, k)) else innerAcc) acc [1..nLimit]



main :: IO()
main = do
    let limit = 2000000
    print $ findMinArea limit
