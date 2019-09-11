import Data.Char (digitToInt)


main :: IO()
main = do
    print $ foldl (\acc c -> acc + digitToInt c) 0 $ show $ 2 ^ 1000
