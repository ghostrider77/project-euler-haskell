import Data.List (permutations, sort)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V


lineIndices :: [[Int]]
lineIndices = [[0, 1, 2], [3, 2, 4], [5, 4, 6], [7, 6, 8], [9, 8, 1]]


stringRepresentation :: Vector Int -> String
stringRepresentation permutation =
    let doubleList = lineIndices ++ lineIndices
        size = length lineIndices
        cyclicalIndices = [take size $ drop k doubleList | k <- [0..(size-1)]]
        intLists = [concatMap (map (permutation !)) indices | indices <- cyclicalIndices]
    in concatMap show $ minimum intLists


maximum16DigitString :: [Int] -> String
maximum16DigitString numbers =
    let perms = map V.fromList $ permutations numbers
        isValid :: Vector Int -> Bool
        isValid permutation =
            let lineSum = V.sum $ V.take 3 permutation
            in all (\indices -> lineSum == sum (map (permutation !) indices)) lineIndices
        validPerms = filter isValid perms
    in maximum $ filter (\s -> length s == 16) $ map stringRepresentation validPerms


main :: IO()
main = do
    let numbers = [1..10]
    putStrLn $ maximum16DigitString numbers
