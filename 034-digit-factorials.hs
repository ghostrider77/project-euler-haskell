import Data.Char (digitToInt)
import Data.Vector (Vector, fromList, (!))


digitFactorials :: Vector Int
digitFactorials = fromList $ go [] 0
    where
        go :: [Int] -> Int -> [Int]
        go factorials k
            | k == 10 = reverse factorials
            | otherwise = let fact = product [1..k] in go (fact : factorials) (k + 1)


digitFactorialSum :: Int -> Int
digitFactorialSum limit = foldl (\acc k -> if suitable k then acc + k else acc) 0 [10..limit]
    where
        suitable :: Int -> Bool
        suitable k = let digits = map digitToInt (show k) in k == sum (map (digitFactorials !) digits)


main :: IO()
main = do
    let limit = 999999
    print $ digitFactorialSum limit
