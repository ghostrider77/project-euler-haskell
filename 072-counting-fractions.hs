import Data.Vector (Vector, freeze, generate, thaw)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad (forM_)


nrProperReducedFractions :: Int -> Int
nrProperReducedFractions limit =
    let phis = runST $ do
        let size = limit + 1
        stateVector <- thaw (generate size id)
        calcPhiValuesUsingSieve stateVector limit
        freeze stateVector
    in V.sum $ V.drop 2 phis


calcPhiValuesUsingSieve :: (MV.MVector s Int) -> Int -> ST s ()
calcPhiValuesUsingSieve v limit =
    forM_ [2..limit] $ \ix -> do
        x <- MV.read v ix
        if x == ix then do
            let indices = takeWhile (<= limit) $ map (*x) [1..]
            forM_ indices $ \jy -> do
                MV.modify v (\y -> (y - (y `div` x))) jy
        else return ()


main :: IO()
main = do
    let limit = 1000000
    print $ nrProperReducedFractions limit
