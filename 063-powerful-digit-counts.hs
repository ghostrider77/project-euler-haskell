powerfulDigitCount :: Int -> Int
powerfulDigitCount limit = foldl findPowers 0 [1..limit]
    where
        findPowers :: Int -> Int -> Int
        findPowers acc n =
            let nthPowers = map (^ n) [1..9]
                nthPowersWithLengthN = filter (\p -> (length . show) p == n) nthPowers
            in acc + length nthPowersWithLengthN


main :: IO()
main = do
    let limit = ceiling $ logBase (10 / 9) 10
    print $ powerfulDigitCount limit
