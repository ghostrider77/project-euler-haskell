collatzChain :: Int -> [Int]
collatzChain n
    | n == 1 = [1]
    | even n = n : collatzChain (n `div` 2)
    | otherwise = n : collatzChain ((3*n + 1) `div` 2)


longestCollatzChain :: Int -> Int
longestCollatzChain limit = fst $ foldl processNext (1, 1) [2..(limit-1)]
    where
        processNext :: (Int, Int) -> Int -> (Int, Int)
        processNext acc @ (longestN, longestSize) k =
            let size = length $ collatzChain k
            in if size > longestSize then (k, size) else acc


main :: IO()
main = do
    let limit = 1000000
    print $ longestCollatzChain limit
