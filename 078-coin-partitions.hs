import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M


modulus :: Int
modulus = 1000000


calcPartitionNumber :: IntMap Int -> Int -> Int
calcPartitionNumber cache n = go 0 1
    where
        defaultValue = 0
        go :: Int -> Int -> Int
        go acc k =
            let arg1 = n - k*(3*k - 1) `div` 2
                arg2 = n - k*(3*k + 1) `div` 2
                sign = if even k then -1 else 1
                p1 = sign * (M.findWithDefault defaultValue arg1 cache)
                p2 = sign * (M.findWithDefault defaultValue arg2 cache)
                acc' = (acc + p1 + p2) `mod` modulus
            in if arg2 < 0 then acc' else go acc' (k + 1)


divisiblePartitionNumber :: Int -> Int
divisiblePartitionNumber modulus = go (M.singleton 0 1) 1
    where
        go :: IntMap Int -> Int -> Int
        go acc n =
            let pn = calcPartitionNumber acc n
            in if pn == 0 then n else go (M.insert n pn acc) (n + 1)


main :: IO()
main = do
    print $ divisiblePartitionNumber modulus
