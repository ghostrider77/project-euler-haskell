import Data.Array (Array(..), listArray, (!), range)


calcBinomCoefficientsExceedingLimit :: Int -> Int -> Int
calcBinomCoefficientsExceedingLimit nMax limit = foldl (\acc x -> if x == 0 then acc + 1 else acc) 0 pascalTriangle
    where
        clippedBinomialCoefficent :: Int -> Int -> Int
        clippedBinomialCoefficent 0 0 = 1
        clippedBinomialCoefficent n r
            | r > n = -1
            | r == 0 || n == r = 1
            | otherwise =
                let a = pascalTriangle ! (n - 1, r - 1)
                    b = pascalTriangle ! (n - 1, r)
                    clippedBinomCoeff = if (a == 0 || b == 0) then 0 else a + b
                in if clippedBinomCoeff > limit then 0 else clippedBinomCoeff
        bounds = ((0, 0), (nMax, nMax))
        pascalTriangle = listArray bounds [clippedBinomialCoefficent ix jy | (ix, jy) <- range bounds]


main :: IO()
main = do
    let nMax = 100
    let limit = 1000000
    print $ calcBinomCoefficientsExceedingLimit nMax limit
