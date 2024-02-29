coinSums :: Int -> [Int] -> Int
coinSums 0 _ = 1
coinSums _ [] = 0
coinSums amount (coin : rest) = foldl (\acc amount' -> acc + coinSums amount' rest) 0 [amount,(amount-coin)..0]


main :: IO()
main = do
    let coins = reverse [1, 2, 5, 10, 20, 50, 100, 200]
    print $ coinSums 200 coins
