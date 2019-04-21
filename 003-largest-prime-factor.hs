removeFactor :: Int -> Int -> Int
removeFactor n p = if n `mod` p == 0 then removeFactor (n `div` p) p else n


findLargestPrimeFactor :: Int -> Int
findLargestPrimeFactor n =
    let n' = removeFactor n 2
        start = if even n then 2 else 1
        upperLimit = floor $ sqrt $ fromIntegral n'
        (rest, p) =
            foldl (\(k, acc) l -> if k `mod` l /= 0 then (k, acc) else (removeFactor k l, l))
                  (n', start)
                  [3,5..upperLimit]
    in if rest > 1 then n' else p


main :: IO()
main = do
    let n = 600851475143
    let result = findLargestPrimeFactor n
    print result
