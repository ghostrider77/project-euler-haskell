import Data.List (subsequences)
import Data.Set (Set)
import qualified Data.Set as S


combinations :: Int -> [a] -> [[a]]
combinations k = filter ((== k) . length) . subsequences


suitableCubes :: Set Char -> Set Char -> [String] -> Bool
suitableCubes _ _ [] = True
suitableCubes cube1 cube2 ([a, b] : rest) =
    let b1 = S.member a cube1 && S.member b cube2
        b2 = S.member a cube2 && S.member b cube1
        squareCanBeShown = b1 || b2
    in if squareCanBeShown then suitableCubes cube1 cube2 rest else False


numberOfDiceArrangements :: [Char] -> Int
numberOfDiceArrangements digits = foldl processArrangements 0 $ zip cubeArrangements [1..]
    where
        cubeArrangements = combinations 6 digits
        squares = ["01", "04", "09", "16", "25", "36", "49", "64", "81"]
        addMirroredNumber :: Set Char -> Set Char
        addMirroredNumber cube
            | S.member '6' cube = S.insert '9' cube
            | S.member '9' cube = S.insert '6' cube
            | otherwise = cube
        processArrangements :: Int -> ([Char], Int) -> Int
        processArrangements count (cube1, n) =
            let c1 = addMirroredNumber $ S.fromList cube1
                innerLoop :: Int -> [Char] -> Int
                innerLoop acc cube2 =
                    let c2 = addMirroredNumber $ S.fromList cube2
                    in if suitableCubes c1 c2 squares then acc + 1 else acc
            in foldl innerLoop count $ drop n cubeArrangements


main :: IO()
main = do
    let digits = ['0'..'9']
    print $ numberOfDiceArrangements digits
