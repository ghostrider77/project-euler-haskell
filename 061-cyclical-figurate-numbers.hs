import Data.List (permutations)


calcFigurateNumbers :: Int -> Int -> [[String]]
calcFigurateNumbers lower upper =
    let restrict :: [Int] -> [Int]
        restrict = dropWhile (< lower) . takeWhile (< upper)
        triangles = restrict $ map (\n -> n*(n+1) `div` 2) [1..]
        squares = restrict $ map (^2) [1..]
        pentagonals = restrict $ map (\n -> n*(3*n-1) `div` 2) [1..]
        hexagonals = restrict $ map (\n -> n*(2*n-1)) [1..]
        heptagonals = restrict $ map (\n -> n*(5*n-3) `div` 2) [1..]
        octagonals = restrict $ map (\n -> n*(3*n-2)) [1..]
        figurateNumbers = [triangles, squares, pentagonals, hexagonals, heptagonals, octagonals]
    in map (map show) figurateNumbers


generateCandidateSolutions :: [[String]] -> [[String]]
generateCandidateSolutions figurateNumbers =
    let extend :: [String] -> [String] -> [[String]]
        extend candidate figurates =
            let lastNumber = head candidate
                nextNumbers = filter (\n -> (not $ n `elem` candidate) && drop 2 lastNumber == take 2 n) figurates
            in map (\n -> n : candidate) nextNumbers
        go :: [[String]] -> [[String]] -> [[String]]
        go candidates [] = candidates
        go candidates (figurates : rest) =
            let candidates' = concatMap (\candidate -> extend candidate figurates) candidates
            in go candidates' rest
        candidateFigurateNumbers = go (map return $ head figurateNumbers) (tail figurateNumbers)
    in filter (\candidates -> drop 2 (head candidates) == take 2 (last candidates)) candidateFigurateNumbers


findCyclicalFigurateNumbers :: Int -> Int -> Int
findCyclicalFigurateNumbers lower upper =
    let figurateNumbers = calcFigurateNumbers lower upper
        allPossibleFigurateOrder = permutations figurateNumbers
        candidates = concatMap generateCandidateSolutions allPossibleFigurateOrder
        solution = head $ candidates
    in sum $ map read solution


main :: IO()
main = do
    let lower  = 1000
    let upper = 10000
    print $ findCyclicalFigurateNumbers lower upper
