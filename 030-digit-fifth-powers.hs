powerDigitSum :: Int -> Int -> Int
powerDigitSum k number = go 0 number
    where
        go :: Int -> Int -> Int
        go acc 0 = acc
        go acc n = go (acc + (n `mod` 10) ^ k) (n `div` 10)


solveDigitFifthPowerProblem :: Int
solveDigitFifthPowerProblem = foldl (\acc n -> if n == powerDigitSum 5 n then acc + n else acc) 0 [10..354294]


main :: IO()
main = do
    print $ solveDigitFifthPowerProblem
