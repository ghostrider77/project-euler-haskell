import Data.IntSet (IntSet)
import qualified Data.IntSet as S


firstMultigonalNumber :: Int -> Int -> Int
firstMultigonalNumber = go singletonSet
    where
        singletonSet = S.singleton 40755
        go :: IntSet -> Int -> Int -> Int
        go hexagonals nH nP =
            let nH' = nH + 1
                nP' = nP + 1
                p = nP' * (3*nP' - 1) `div` 2
                h = nH' * (2*nH' - 1)
            in if S.member p hexagonals then p else go (S.insert h hexagonals) nH' nP'


main :: IO()
main = do
    let n_h = 143
    let n_p = 165
    print $ firstMultigonalNumber n_h n_p
