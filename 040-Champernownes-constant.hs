import Data.Char (digitToInt)
import Data.Vector (fromList, (!))
import qualified Data.Vector as V


champernownesConstantProduct :: [Int] -> Int
champernownesConstantProduct indices =
    let len = last indices + 1
        constant = V.take len $ fromList $ concat $ map show [0..]
    in product $ map (digitToInt . (constant !)) indices


main :: IO()
main = do
    let indices = map (10 ^) [0..6]
    print $ champernownesConstantProduct indices
