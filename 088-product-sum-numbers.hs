import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as S
import Data.List (find, sort, nub)

type Decomposition = [Int]


decompose :: M.Map Int [Decomposition] -> Int -> M.Map Int [Decomposition]
decompose acc k =
    let limit = floor $ sqrt $ fromIntegral k
        processNext :: M.Map Int [Decomposition] -> Int -> M.Map Int [Decomposition]
        processNext acc' p
            | k `mod` p /= 0 = acc'
            | otherwise =
                let d = k `div` p
                    decompositionsOfD = acc' M.! d
                    decompositionsOfP = acc' M.! p
                    merged = nub [sort (a ++ b) | a <- decompositionsOfD, b <- decompositionsOfP]
                in M.insertWith (++) k merged acc'
    in foldl processNext acc [2..limit]


calcDecompositions :: Int -> M.Map Int [Decomposition]
calcDecompositions n =
    let trivialDecompositions = M.fromDistinctAscList $ map (\k -> (k, [[k]])) [2..n]
    in foldl decompose trivialDecompositions [2..n]


sumOfMinimalProductSumNumbers :: Int -> Int
sumOfMinimalProductSumNumbers limit =
    let doubleLimit = 2*limit
        distinctDecompositions = calcDecompositions doubleLimit
        minimalNumbers = IM.fromDistinctAscList $ map (\n -> (n, maxBound)) [1..limit]
        processNumbers :: IntMap Int -> Int -> IntMap Int
        processNumbers acc n =
            let decompositions = distinctDecompositions M.! n
                updateMinimalNumberMap :: IntMap Int -> Decomposition -> IntMap Int
                updateMinimalNumberMap mapping factors =
                    let units = n - (sum factors)
                        k = length factors + units
                    in if k <= limit && mapping IM.! k > n then IM.insert k n mapping else mapping
            in foldl updateMinimalNumberMap acc decompositions
        result = IM.filterWithKey (\k _ -> k /= 1) $ foldl processNumbers minimalNumbers [2..doubleLimit]
    in S.foldl (+) 0 $ S.fromList $ IM.elems result


main :: IO()
main = do
    let limit = 12000
    print $ sumOfMinimalProductSumNumbers limit
