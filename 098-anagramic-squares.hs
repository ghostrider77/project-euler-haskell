import Data.List.Split (splitOn)
import Data.List (sortOn, sort, subsequences, nub, permutations)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M


combinations :: Int -> [a] -> [[a]]
combinations k = filter ((== k) . length) . subsequences


extendAnagramPairs :: [String] -> [(String, String)] -> [(String, String)]
extendAnagramPairs ws acc =
    let anagrams = do
        (w1, ix) <- zip ws [1..]
        let sw1 = sort w1
        w2 <- drop ix ws
        guard (sw1 == sort w2)
        return (w1, w2)
    in anagrams ++ acc


calcAnagramPairs :: [String] -> [(String, String)]
calcAnagramPairs ws = go 1 (sortOn length ws) []
    where
        go :: Int -> [String] -> [(String, String)] -> [(String, String)]
        go _ [] acc = acc
        go n sortedWords acc =
            let (wordsSizeN, rest) = span ((== n) . length) sortedWords
            in go (n + 1) rest (extendAnagramPairs wordsSizeN acc)


encodeWithNumbers :: String -> String -> Set String -> Int
encodeWithNumbers w1 w2 squares =
    let letters = nub w1
        nrLetters = length letters
        replacements = concatMap permutations $ combinations nrLetters ['0'..'9']
        checkEncoding :: Int -> String -> Int
        checkEncoding maxSquare replacement =
            let mapping = M.fromList $ zip letters replacement
                encodedW1 = map (mapping M.!) w1
                encodedW2 = map (mapping M.!) w2
                condition = S.member encodedW1 squares && S.member encodedW2 squares
            in if condition then maximum [read encodedW1, read encodedW2, maxSquare] else maxSquare
    in foldl checkEncoding 0 replacements


largestAnagramicSquare :: [String] -> Int
largestAnagramicSquare content =
    let anagramPairs = calcAnagramPairs content
        squareStrings = S.map show $ S.fromAscList $ takeWhile (<= 10^10) $ map (^2) [1..]
    in foldl (\maxSquare (w1, w2) -> max (encodeWithNumbers w1 w2 squareStrings) maxSquare) 0 anagramPairs


main :: IO()
main = do
    content <- fmap (splitOn ",") (readFile "../resources/P0098.dat")
    let ws = map (filter (/= '"')) content
    print $ largestAnagramicSquare ws
