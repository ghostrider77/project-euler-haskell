import Data.Char (digitToInt)
import Data.Vector ((!), fromList)
import Data.IntSet (IntSet)
import qualified Data.IntSet as S


nrFactorialChains :: Int -> Int -> Int
nrFactorialChains size limit =
    let factorials = fromList $ scanl (*) 1 [1..9]
        chainLength :: Int -> Int
        chainLength = factorialChain S.empty
        factorialChain :: IntSet -> Int -> Int
        factorialChain chain k
            | S.member k chain = S.size chain
            | otherwise =
                let digitFactorialSum = sum $ map ((factorials !) . digitToInt) (show k)
                in factorialChain (S.insert k chain) digitFactorialSum
    in foldl (\acc n -> if chainLength n == size then acc + 1 else acc) 0 [1..limit]


main :: IO()
main = do
    let limit = 1000000
    let size = 60
    print $ nrFactorialChains size limit
