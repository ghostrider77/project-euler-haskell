squareOfDigits :: Int -> Int
squareOfDigits = go 0
    where
        go :: Int -> Int -> Int
        go acc 0 = acc
        go acc n = go (acc + (n `mod` 10) ^ 2) (n `div` 10)


solveProblem :: Int -> Int
solveProblem limit = foldl (\acc k -> if (arrivesAtEightyNine k) then acc + 1 else acc) 0 [1..limit]
    where
        arrivesAtEightyNine :: Int -> Bool
        arrivesAtEightyNine k
            | k == 1 = False
            | k == 89 = True
            | otherwise = arrivesAtEightyNine $ squareOfDigits k


main :: IO()
main = do
    let limit = 10000000
    print $ solveProblem limit
