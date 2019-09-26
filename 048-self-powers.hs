import Text.Printf (printf)


modulus :: Int
modulus = 10000000000


solveLastDigitProblem :: Int -> Int
solveLastDigitProblem n = foldl (\acc k -> (acc + selfPower k) `mod` modulus) 0 [1..n]
    where
        selfPower :: Int -> Int
        selfPower k = foldl (\acc _ -> (acc * k) `mod` modulus) 1 [1..k]


main :: IO()
main = do
    let n = 1000
    printf "%010d\n" $ solveLastDigitProblem n
