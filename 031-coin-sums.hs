coinSums :: Int -> [Int] -> Int
coinSums 0 _ = 1
coinSums _ [] = 0
coinSums amount (coin : rest) = go 0 0
    where
        go :: Int -> Int -> Int
        go acc k =
            let amount' = amount - k * coin
            in if amount' < 0 then acc else go (acc + coinSums amount' rest) (k + 1)


main :: IO()
main = do
    let coins = reverse [1, 2, 5, 10, 20, 50, 100, 200]
    print $ coinSums 200 coins
