import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Ord (Down(..))
import System.IO

data Number = Number { base :: Int, exponent :: Int } deriving Eq

instance Ord Number where
    Number b1 e1 <= Number b2 e2 =
        (fromIntegral e1) * log (fromIntegral b1) <= (fromIntegral e2) * log (fromIntegral b2)


lineToNumber :: String -> Number
lineToNumber line =
    let [b, e] = map read $ splitOn "," line in Number b e


findIndexOfLargest :: [Number] -> Int
findIndexOfLargest numbers = snd $ head $ sortBy (\(n, _) (m, _) -> compare (Down n) (Down m)) $ zip numbers [1..]


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0099_base_exp.dat")
    let numbers = map lineToNumber content
    print $ findIndexOfLargest numbers
