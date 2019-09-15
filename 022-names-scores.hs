import Data.List.Split (splitOn)
import Data.List (sort, elemIndex)


calcNameScores :: [String] -> Int
calcNameScores names = foldl (\acc (name, ix) -> acc + ix * (nameValue name)) 0 (zip sortedNames [1..])
    where
        sortedNames = sort names
        alphabet = ['A'..'Z']
        indexOf :: String -> Char -> Int
        indexOf string c = case elemIndex c string of Just ix -> ix + 1
                                                      Nothing -> 0
        nameValue :: String -> Int
        nameValue name = foldl (\acc c -> acc + indexOf alphabet c) 0 name


main :: IO()
main = do
    names <- fmap (splitOn ",") (readFile "../resources/P0022.dat")
    print $ calcNameScores names
