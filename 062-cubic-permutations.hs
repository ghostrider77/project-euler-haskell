import Data.List (sort)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M


checkNDigitCubes :: Int -> Int -> Maybe Int
checkNDigitCubes n nrPermutations =
    let lowerLimit = ceiling $ 10 ** (fromIntegral (n - 1) / 3)
        upperLimit = if n `mod` 3 == 0 then 10 ^ (n `div` 3) - 1 else floor $ 10 ** (fromIntegral n / 3)
        processNextBase :: Int -> Map String (Int, Int) -> Maybe Int
        processNextBase k acc
            | k <= upperLimit =
                let kCubeDigits = sort $ show $ k ^ 3
                    acc' = M.insertWith (\(_, newCnt) (base, oldCnt) -> (base, oldCnt + newCnt)) kCubeDigits (k, 1) acc
                in processNextBase (k + 1) acc'
            | otherwise =
                let bases = sort $ map (fst . snd) $ M.toList $ M.filter (\(_, c) -> c == nrPermutations) acc
                in case bases of [] -> Nothing
                                 base : _ -> Just (base ^ 3)
    in processNextBase lowerLimit M.empty


smallestCubePermutation :: Int -> Int
smallestCubePermutation nrPermutations =
    let go :: Int -> Int
        go n = case checkNDigitCubes n nrPermutations of Nothing -> go (n + 1)
                                                         Just cube -> cube
    in go 1


main :: IO()
main = do
    let nrPermutations = 5
    print $ smallestCubePermutation nrPermutations
