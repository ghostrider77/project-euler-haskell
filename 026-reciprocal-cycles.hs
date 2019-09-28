import Data.List (elemIndex)


decimalRecurringCycleLength :: Int -> Int
decimalRecurringCycleLength k = go [] 1
    where
        go :: [Int] -> Int -> Int
        go remainders n =
            let r = n `mod` k
            in case elemIndex r remainders of Nothing -> go (r : remainders) (10 * r)
                                              Just ix -> ix + 1


longestRecurringCycle :: Int -> Int
longestRecurringCycle limit = snd $ foldl processElem (1, 1) [2..limit]
    where
        processElem :: (Int, Int) -> Int -> (Int, Int)
        processElem acc @ (maxRecurrence, maxNumber) k =
            let cycleLength = decimalRecurringCycleLength k
            in if cycleLength > maxRecurrence then (cycleLength, k) else acc


main :: IO()
main = do
    let limit = 1000
    print $ longestRecurringCycle limit
