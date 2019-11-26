data Fraction = Fraction { nominator :: Int, denominator :: Int }


orderedFraction :: Int -> Int
orderedFraction limit = a `div` (gcd a b)
    where
        Fraction a b = snd $ foldl processDenominators (1.0, Fraction 0 0) [1..limit]
        threeOverSeven = fromInteger 3 / fromInteger 7
        processDenominators :: (Double, Fraction) -> Int -> (Double, Fraction)
        processDenominators (minDist, Fraction nMin mMin) d =
            let n = (3 * d) `div` 7
                n' = if 7 * n == 3 * d then n - 1 else n
                distance = threeOverSeven - (fromIntegral n' / fromIntegral d)
                (minDist', nMin', mMin') = if distance < minDist then (distance, n', d) else (minDist, nMin, mMin)
            in (minDist', Fraction nMin' mMin')


main :: IO()
main = do
    let limit = 1000000
    print $ orderedFraction limit
