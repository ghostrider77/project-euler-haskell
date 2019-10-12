import qualified Data.IntMap.Strict as M


perimeterWithMostPythagoreanTriples :: Int -> Int
perimeterWithMostPythagoreanTriples limit =
    let perimeters = filter (<= limit) [a + b + c | c <- [5..limit], b <- [1..(c-1)], a <- [1..b], a^2 + b^2 == c^2]
        counter = foldl (\acc p -> let c = M.findWithDefault 0 p acc in M.insert p (c + 1) acc) M.empty perimeters
    in fst $ M.foldlWithKey (\acc @ (_, maxCount) p c -> if c > maxCount then (p, c) else acc) (0, 0) counter


main :: IO()
main = do
    let limit = 1000
    print $ perimeterWithMostPythagoreanTriples limit
