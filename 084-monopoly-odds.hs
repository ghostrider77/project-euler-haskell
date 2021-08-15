import Data.List (find, sortBy)
import Data.Ord (Down(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import System.Random (StdGen, randomR, mkStdGen)

newtype Counter = Counter Int

newtype Dice = Dice Int


data GameState = GameState { position :: Int
                           , randomState :: StdGen
                           , ccCard :: Int
                           , chanceCard :: Int
                           , consecutiveDoubles :: Counter
                           , visited :: Map Int Int
                           }


rollTheDice :: Dice -> StdGen -> (Int, StdGen)
rollTheDice (Dice k) = randomR (1, k)


checkDouble :: Counter -> (Int, Int) -> Counter
checkDouble (Counter c) (d1, d2)
    | d1 == d2 = Counter (min (c + 1) 3)
    | otherwise = Counter 0


isLast3Double :: Counter -> Bool
isLast3Double (Counter c) = c == 3


findNext :: [Int] -> Int -> Int
findNext ps position = case find (> position) ps of Nothing -> head ps
                                                    Just p -> p


modifyPositionAtCommunityChest :: Int -> Int -> Int
modifyPositionAtCommunityChest position card
    | card == 1 = 0
    | card == 2 = 10
    | otherwise = position


modifyPositionAtChance :: Int -> Int -> Int
modifyPositionAtChance position card
    | card == 1 = 0
    | card == 2 = 10
    | card == 3 = 11
    | card == 4 = 24
    | card == 5 = 39
    | card == 6 = 5
    | card == 7 || card == 8 = findNext [5, 15, 25, 35] position
    | card == 9 = findNext [12, 28] position
    | card == 10 = (position - 3) `mod` 40
    | otherwise = position


adjustBoardPosition :: Int -> Counter -> Int -> Int -> (Int, Int, Int)
adjustBoardPosition position counter ccCard chanceCard
    | position == 30 || isLast3Double counter = (10, ccCard, chanceCard)
    | position `elem` [2, 17, 33] = (modifyPositionAtCommunityChest position ccCard, ccCard `mod` 16 + 1, chanceCard)
    | position `elem` [7, 22, 36] = (modifyPositionAtChance position chanceCard, ccCard, chanceCard `mod` 16 + 1)
    | otherwise = (position, ccCard, chanceCard)


mostCommonPositions :: Map Int Int -> String
mostCommonPositions counts =
    let assocList = sortBy (\(_, c1) (_, c2) -> compare (Down c1) (Down c2)) $ M.toList counts
    in concatMap (\x -> if x < 10 then "0" ++ show x else show x) $ map fst $ take 3 $ assocList


playMonopoly :: Int -> Int -> String
playMonopoly n diceSides =
    let dice = Dice diceSides
        playRound :: GameState -> GameState
        playRound (GameState previousPosition gen ccCard chanceCard doubles visited) =
            let (d, gen') = rollTheDice dice gen
                (d', gen'') = rollTheDice dice gen'
                counter = checkDouble doubles (d, d')
                position = (previousPosition + d + d') `mod` 40
                (updatedPosition, ccCard', chanceCard') = adjustBoardPosition position counter ccCard chanceCard
                visited' = M.insertWith (+) updatedPosition 1 visited
            in GameState updatedPosition gen'' ccCard' chanceCard' counter visited'
        gen = mkStdGen 2112
        finalState = foldl (\state _ -> playRound state) (GameState 0 gen 1 1 (Counter 0) M.empty) [1..n]
    in mostCommonPositions (visited finalState)


main :: IO()
main = do
    let n = 1000000
    let diceSides = 4
    putStrLn $ playMonopoly n diceSides
