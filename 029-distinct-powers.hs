import Data.List (nub)


distinctPowers :: Integer -> [Integer]
distinctPowers limit = nub [a ^ b | a <- [2..limit], b <- [2..limit]]


main :: IO()
main = do
    let limit = 100
    print $ length $ distinctPowers limit
