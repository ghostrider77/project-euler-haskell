import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Array (Array, listArray, (!), range)

type Node = (Int, Int)


convertToMatrix :: [String] -> Int -> Array Node Int
convertToMatrix rows n =
    let convertToIntList :: String -> [Int]
        convertToIntList = map read . (splitOn ",")
    in listArray ((0, 0), (n - 1, n - 1)) $ concat $ map convertToIntList rows


updateDistances :: Array Node Int -> Int -> Node -> Int -> Map Node Int -> Map Node Int -> (Map Node Int, Map Node Int)
updateDistances matrix n (i, j) distU distances t = go neighbours distances t
    where
        validNode :: Node -> Bool
        validNode (i, j) = i >= 0 && i < n && j >= 0 && j < n
        neighbours = filter validNode [(i - 1, j), (i + 1, j), (i, j + 1)]
        go :: [Node] -> Map Node Int -> Map Node Int -> (Map Node Int, Map Node Int)
        go [] distances' t' = (distances', t')
        go (v @ (vi, vj) : rest) distances' t' =
            let distanceThroughU = distU + matrix ! (vi, vj)
                (distances'', t'') =
                    if (distances' M.! v) <= distanceThroughU then (distances', t')
                    else (M.insert v distanceThroughU distances', M.insert v distanceThroughU t')
            in go rest distances'' t''


shortestPath :: Array Node Int -> Int -> Int
shortestPath matrix n =  M.foldl min maxBound $ M.filterWithKey (\(_, j) _ -> j == n - 1) distances
    where
        bounds = ((0, 0), (n - 1, n - 1))
        initialDistances = M.fromList [((i, j), if j == 0 then matrix ! (i, j) else maxBound) | (i, j) <- range bounds]
        updateablePoints = initialDistances
        distances = runDijkstrasAlgorithm initialDistances updateablePoints
        findNodeWithSmallestDistance :: Map Node Int -> (Node, Int)
        findNodeWithSmallestDistance m =
            M.foldlWithKey
                (\acc @ (minNode, minDist) node d -> if d < minDist then (node, d) else acc) ((0, 0), maxBound) m
        runDijkstrasAlgorithm :: Map Node Int -> Map Node Int -> Map Node Int
        runDijkstrasAlgorithm distances t
            | M.null t = distances
            | otherwise =
                let (u, distU) = findNodeWithSmallestDistance t
                    t' = M.delete u t
                    (distances', t'') = updateDistances matrix n u distU distances t'
                in runDijkstrasAlgorithm distances' t''


main :: IO()
main = do
    content <- fmap lines (readFile "../resources/P0082.dat")
    let n = length content
    let matrix = convertToMatrix content n
    print $ shortestPath matrix n
