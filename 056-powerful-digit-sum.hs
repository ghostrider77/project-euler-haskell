import Data.Char (digitToInt)


calcMaximalDigitSum :: Integer -> Int
calcMaximalDigitSum limit = foldl (\acc n -> let s = digitSum n in if s > acc then s else acc) 0 powers
    where
        powers = [a ^ b | a <- [1..(limit-1)], b <- [1..(limit-1)]]
        digitSum :: Integer -> Int
        digitSum n = sum $ map digitToInt $ show n


main :: IO()
main = do
    let limit = 100
    print $ calcMaximalDigitSum limit
