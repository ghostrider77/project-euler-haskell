import Data.Char (digitToInt)
import Data.Vector (fromList, (!))


champernownesConstantProduct :: [Int] -> Int
champernownesConstantProduct indices =
    let len = last indices + 1
        constant = fromList $ take len $ concatMap show [0..]
    in product $ map (digitToInt . (constant !)) indices


main :: IO()
main = do
    let indices = map (10 ^) [0..6]
    print $ champernownesConstantProduct indices
