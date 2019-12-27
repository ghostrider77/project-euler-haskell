isPalindrome :: Integer -> Integer -> Bool
isPalindrome n b = reversed == n
    where
        reversed = go n 0
        go :: Integer -> Integer -> Integer
        go k reversed
            | k == 0 = reversed
            | otherwise = go (k `div` b) (b*reversed + k `mod` b)


numberOfLychrelNumbers :: Int -> Int
numberOfLychrelNumbers limit = foldl (\acc n -> if isLychrel n then acc + 1 else acc) 0 [1..(limit-1)]
    where
        isLychrel :: Int -> Bool
        isLychrel n = go (toInteger n) 0
        go :: Integer -> Int -> Bool
        go acc k
            | k >= 50 = True
            | otherwise =
                let reversed = read $ reverse $ show acc
                    s = acc + reversed
                in if isPalindrome s 10 then False else go s (k + 1)


main :: IO()
main = do
    let limit = 10000
    print $ numberOfLychrelNumbers limit
