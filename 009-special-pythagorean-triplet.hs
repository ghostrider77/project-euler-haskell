import Control.Monad (guard)


getPythagoreanTriplets :: Int -> [(Int, Int, Int)]
getPythagoreanTriplets s = do
    a <- [1..(s-1)]
    b <- [(a+1)..(s-1)]
    guard (a + b <= s - 1)
    let c = s - (a + b)
    guard (c^2 - (a^2 + b^2) == 0)
    return (a, b, c)


main :: IO()
main = do
    let s = 1000
    let (a, b, c) =  head $ getPythagoreanTriplets s
    print (a * b * c)
