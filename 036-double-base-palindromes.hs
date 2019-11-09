isPalindrome :: Int -> Int -> Bool
isPalindrome n b = reversed == n
    where
        reversed = go n 0
        go :: Int -> Int -> Int
        go k reversed
            | k == 0 = reversed
            | otherwise = go (k `div` b) (b*reversed + k `mod` b)


sumOfDoublePalindromes :: Int -> Int
sumOfDoublePalindromes limit = foldl (\acc n -> if doublePalindrome n then acc + n else acc) 0 [1,3..limit]
    where
        doublePalindrome :: Int -> Bool
        doublePalindrome n = isPalindrome n 10 && isPalindrome n 2


main :: IO()
main = do
    let limit = 1000000
    print $ sumOfDoublePalindromes limit
