import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.IntSet as S
import qualified Data.Map.Strict as M
import Data.Char (digitToInt)

type Cell = (Int, Int)
newtype Puzzle = Puzzle (Map Cell Int)

boardSize :: Int
boardSize = 9


squareSize :: Int
squareSize = 3


readPuzzle :: [String] -> Puzzle
readPuzzle gridLines =
    let processLine :: Map Cell Int -> (String, Int) -> Map Cell Int
        processLine acc (line, i) =
            let lineMap = map (\(c, j) -> ((i, j), digitToInt c)) $ zip line [0..]
            in M.union acc $ M.fromList lineMap
    in Puzzle $ foldl processLine M.empty $ zip gridLines [0..]


readSuDokuPuzzles :: [String] -> [Puzzle]
readSuDokuPuzzles [] = []
readSuDokuPuzzles content =
    let (currentGrid, rest) = splitAt (boardSize + 1) content
        puzzle = readPuzzle $ drop 1 currentGrid
    in puzzle : readSuDokuPuzzles rest


getEmptyCells :: Puzzle -> [(Cell, [Int])]
getEmptyCells puzzle @ (Puzzle board) =
    let getPossibleValues :: Cell -> [Int]
        getPossibleValues cell @ (x0, y0) =
            case board M.! cell of
                0 ->
                    let row = S.fromList $ map (\y -> board M.! (x0, y)) [0..(boardSize-1)]
                        col = S.fromList $ map (\x -> board M.! (x, y0)) [0..(boardSize-1)]
                        x = x0 `div` squareSize
                        y = y0 `div` squareSize
                        xs = [squareSize*x..(squareSize*x+squareSize-1)]
                        ys = [squareSize*y..(squareSize*y+squareSize-1)]
                        square = S.fromList [board M.! (i, j) | i <- xs, j <- ys]
                    in S.toList $ S.difference (S.fromAscList [1..boardSize]) (S.unions [row, col, square])
                _ -> []
        coords = [0..(boardSize-1)]
        values = [((x, y), getPossibleValues (x, y)) | x <- coords, y <- coords, board M.! (x, y) == 0]
    in sortBy (\(_, lst1) (_, lst2) -> compare (length lst1) (length lst2)) values


solveSuDokuPuzzle :: Puzzle -> [Puzzle]
solveSuDokuPuzzle puzzle @ (Puzzle board) =
    case getEmptyCells puzzle of
        [] -> [puzzle]
        (_, []) : _ -> []
        (cell, possibleValues) : _ -> do
            value <- possibleValues
            let puzzle' = Puzzle $ M.insert cell value board
            solveSuDokuPuzzle puzzle'


solvePuzzles :: [Puzzle] -> Int
solvePuzzles puzzles =
    let go :: Int -> [Puzzle] -> Int
        go acc [] = acc
        go acc (puzzle : rest) =
            case solveSuDokuPuzzle puzzle of
                [Puzzle board] ->
                    let topCornerValue = 100 * (board M.! (0, 0)) + 10 * (board M.! (0, 1)) + (board M.! (0, 2))
                    in go (acc + topCornerValue) rest
                _ -> error "No unique solution was found."
    in go 0 puzzles


main :: IO()
main = do
    content <- lines <$> readFile "../resources/P0096.dat"
    let puzzles = readSuDokuPuzzles content
    print $ solvePuzzles puzzles
