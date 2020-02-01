import Control.Monad (guard)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M


singlularIntegerRightTriangles :: Int -> Int
singlularIntegerRightTriangles limit =
    let upperLimit = floor $ sqrt $ (fromIntegral limit) / 2
        tripletPerimeters = do
            n <- [1..upperLimit]
            m <- [(n+1)..upperLimit]
            guard (((n - m) `mod` 2 /= 0) && (gcd m n == 1))
            let a = m^2 - n^2
            let b = 2*m*n
            let c = m^2 + n^2
            let primitiveTripletPerimeter = a + b + c
            s <- takeWhile (<= limit) $ map (* primitiveTripletPerimeter) [1..]
            return s
        countPerimeters :: IntMap Int -> Int -> IntMap Int
        countPerimeters acc perimeter = M.insertWith (+) perimeter 1 acc
    in M.size $ M.filter (== 1) $ foldl countPerimeters M.empty tripletPerimeters


main :: IO()
main = do
    let limit = 1500000
    print $ singlularIntegerRightTriangles limit
