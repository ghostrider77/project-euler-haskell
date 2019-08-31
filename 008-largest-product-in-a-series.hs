import Data.Char (digitToInt)
import Data.Vector (Vector(..), fromList)
import qualified Data.Vector as V


convertToIntVector :: String -> Vector Int
convertToIntVector line = fromList $ map digitToInt $ filter (/= '\r') line


largestAdjacentProduct :: Int -> Vector Int -> Int
largestAdjacentProduct windowSize numbers = go 0 0
    where
        size = V.length numbers
        go :: Int -> Int -> Int
        go acc start
            | start + windowSize - 1 == size = acc
            | otherwise =
                let prod = V.product $ V.slice start windowSize numbers
                    nextStart = start + 1
                in if prod > acc then go prod nextStart else go acc nextStart


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0008.dat")
    let numbers = V.concat $ map convertToIntVector content
    print $ largestAdjacentProduct 13 numbers
