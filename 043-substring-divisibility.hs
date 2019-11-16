import Data.List (permutations)


generate3DigitNumbers :: [Int] -> [Int]
generate3DigitNumbers digits = go [n0] rest
    where
        (first, rest) = splitAt 3 digits
        n0 = digits2Number first
        go :: [Int] -> [Int] -> [Int]
        go acc [] = tail $ reverse acc
        go acc (d : ds) =
            let h = head acc
                next = (h `mod` 100) * 10 + d
            in go (next : acc) ds


digits2Number :: [Int] -> Int
digits2Number = foldl1 (\acc d -> 10*acc + d)


sumOfPandigitals :: [Int] -> Int
sumOfPandigitals digits = foldl (\acc n -> if conditions n then acc + digits2Number n else acc) 0 pandigitals
    where
        divisors = [2, 3, 5, 7, 11, 13, 17]
        pandigitals = filter (\p -> head p /= 0) $ permutations digits
        conditions :: [Int] -> Bool
        conditions n =
            let threeDigitsNumbers = generate3DigitNumbers n
            in all (\(d, k) -> k `mod` d == 0) $ zip divisors threeDigitsNumbers


main :: IO()
main = do
    let digits = [0..9]
    print $ sumOfPandigitals digits
