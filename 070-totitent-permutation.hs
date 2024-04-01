import Data.List (sort)
import Data.Vector (Vector, (!), freeze, generate, thaw)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_, when)


calcPhiValuesUsingSieve :: Int -> Vector Int
calcPhiValuesUsingSieve limit =
    runST $ do
        let size = limit + 1
        stateVector <- thaw (generate size id)
        updatePhi stateVector limit
        freeze stateVector


updatePhi :: MV.MVector s Int -> Int -> ST s ()
updatePhi v limit =
    forM_ [2..limit] $ \ix -> do
        x <- MV.read v ix
        when (x == ix) $ do
            let indices = takeWhile (<= limit) $ map (*x) [1..]
            forM_ indices $ \jy -> do
                MV.modify v (\y -> y - y `div` x) jy


minimumTotientQuotient :: Int -> Int
minimumTotientQuotient limit = fst $ foldl processNext (1, 2) [3,5..limit]
    where
        phiVector = calcPhiValuesUsingSieve limit
        isPermutation :: Int -> Int -> Bool
        isPermutation n m = sort (show m) == sort (show n)
        processNext :: (Int, Double) -> Int -> (Int, Double)
        processNext acc @ (minN, minRatio) n =
            let phi = phiVector ! n
                ratio = fromIntegral n / fromIntegral phi
            in if ratio >= minRatio || not (isPermutation phi n) then acc else (n, ratio)


main :: IO()
main = do
    let limit = 10000000
    print $ minimumTotientQuotient limit
