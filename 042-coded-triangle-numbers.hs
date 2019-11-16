import Data.List.Split (splitOn)
import qualified Data.IntSet as S
import qualified Data.Map.Strict as M


nrTriangleWords :: [String] -> Int
nrTriangleWords ws = foldl (\acc v -> if S.member v triangles then acc + 1 else acc) 0 wordValues
    where
        letterPositions = M.fromAscList $ zip ['A'..'Z'] [1..]
        wordValues = map calcWordValue ws
        calcWordValue :: String -> Int
        calcWordValue = foldl (\acc c -> acc + M.findWithDefault 0 c letterPositions) 0
        largestValue = maximum wordValues
        triangles = S.fromAscList $ takeWhile (<= largestValue) $ map (\n -> n*(n + 1) `div` 2 ) [1..]


main :: IO()
main = do
    content <- fmap (splitOn ",") (readFile "../resources/P0042.dat")
    print $ nrTriangleWords content
