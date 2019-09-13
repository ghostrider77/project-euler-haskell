import Data.Char (digitToInt)


factorialDigitSum :: Integer -> Int
factorialDigitSum n = foldl (\acc c -> acc + digitToInt c) 0 (show $ product [1..n])


main :: IO()
main = do
    let n = 100
    print $ factorialDigitSum n
