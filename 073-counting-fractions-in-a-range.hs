nrProperReducedFractionsInRange :: Int -> Int
nrProperReducedFractionsInRange limit = foldl (\acc d -> acc + calcNrFractionsInInterval d) 0 [4..limit]
    where
        calcNrFractionsInInterval :: Int -> Int
        calcNrFractionsInInterval d =
            let nMin = d `div` 3 + 1
                nMax = if d `mod` 2 == 0 then d `div` 2 - 1 else d `div` 2
            in foldl (\acc n -> if gcd n d == 1 then acc + 1 else acc) 0 [nMin..nMax]


main :: IO()
main = do
    let limit = 12000
    print $ nrProperReducedFractionsInRange limit
