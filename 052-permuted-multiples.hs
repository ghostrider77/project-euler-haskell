import Data.List (sort)


sameDigitMultiples :: Int -> Int -> Bool
sameDigitMultiples limit n = go 2
    where
        digits = sort $ show n
        go :: Int -> Bool
        go k
            | k > limit = True
            | otherwise =
                let m = k * n
                    digitsOfM = sort $ show m
                in if digits == digitsOfM then go (k + 1) else False


smallestNumberWithPermutedMultiples :: Int -> Int
smallestNumberWithPermutedMultiples limit = go 1
    where
        go :: Int -> Int
        go n
            | sameDigitMultiples limit n = n
            | otherwise = go (n + 1)


main :: IO()
main = do
    let limit = 6
    print $ smallestNumberWithPermutedMultiples limit
