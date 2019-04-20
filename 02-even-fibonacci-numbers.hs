sumOfEvenFibonacci :: Int -> Int
sumOfEvenFibonacci limit =
    let fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)
        evenFibs = filter even fibonacci
    in sum $ takeWhile (< limit) evenFibs


main :: IO()
main = do
    let limit = 4000000
    let result = sumOfEvenFibonacci limit
    print result
