indexOfLargeFibonacciNumber :: Int -> Int
indexOfLargeFibonacciNumber nrDigits = (length $ takeWhile (< nrDigits) $ map (length . show) fibs) + 1
    where fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


main :: IO()
main = do
    let nrDigits = 1000
    print $ indexOfLargeFibonacciNumber nrDigits
