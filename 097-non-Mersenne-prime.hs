import Text.Printf (printf)

modulus :: Int
modulus = 10000000000


moduloExponentiation :: Int -> Int -> Int
moduloExponentiation base exponent = foldl (\acc _ -> (acc * base) `mod` modulus) 1 [1..exponent]


main :: IO()
main = do
    let powerOfTwo = moduloExponentiation 2 7830457
    let result = (28433 * powerOfTwo + 1) `mod` modulus
    printf "%010d" result
