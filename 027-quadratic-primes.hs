data QuadraticPolynomial = P Int Int


isPrime :: Int -> Bool
isPrime n
    | n == 2 = True
    | n == 1 || even n = False
    | otherwise =
        let limit = floor $ sqrt $ fromIntegral n
        in all (\k -> n `mod` k /= 0) [3,5..limit]


primeValueLength :: QuadraticPolynomial -> Int
primeValueLength p = go 0
    where
        go :: Int -> Int
        go n = if isPrime $ abs $ evaluateQuadraticPolynomial p n then go (n + 1) else n


evaluateQuadraticPolynomial :: QuadraticPolynomial -> Int -> Int
evaluateQuadraticPolynomial (P a b) n = n ^ 2 + a*n + b


longestPrimePolynomialValues :: Int -> Int
longestPrimePolynomialValues limit =
    let bounds = [-limit..limit]
        coefficients = [(a, b) | a <- bounds, b <- bounds]
    in snd $ foldl (\acc @ (maxLen, maxProd) (a, b) ->
        let k = primeValueLength (P a b) in if k > maxLen then (k, a*b) else acc) (0, minBound) coefficients


main :: IO()
main = do
    let limit = 1000
    print $ longestPrimePolynomialValues limit
