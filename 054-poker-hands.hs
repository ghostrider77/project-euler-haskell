{-# LANGUAGE TupleSections #-}

import Data.Char(digitToInt)
import Data.List (nub, sortOn, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (Down(..))
import Data.IntMap (fromListWith, toList)

data Suit = Spades | Hearts | Diamonds | Clubs deriving Eq
data Card = Card {suit :: Suit, value :: Int}
newtype Hand = Hand [Card]
data Game = Game Hand Hand

data Rank =
      RoyalFlush
    | StraightFlush Int
    | FourOfAKind Int
    | FullHouse Int Int
    | Flush Int
    | Straight Int
    | ThreeOfAKind Int
    | TwoPairs Int Int Int
    | OnePair Int [Int]
    | HighCard [Int] deriving Eq


instance Ord Rank where
    compare RoyalFlush RoyalFlush = EQ
    compare RoyalFlush _ = GT
    compare (StraightFlush h1) (StraightFlush h2) = compare h1 h2
    compare (StraightFlush _) RoyalFlush = LT
    compare (StraightFlush _) _ = GT
    compare (FourOfAKind v1) (FourOfAKind v2) = compare v1 v2
    compare (FourOfAKind _) RoyalFlush = LT
    compare (FourOfAKind _) (StraightFlush _) = LT
    compare (FourOfAKind _) _ = GT
    compare (FullHouse t1 _) (FullHouse t2 _) = compare t1 t2
    compare (FullHouse _ _) RoyalFlush = LT
    compare (FullHouse _ _) (StraightFlush _) = LT
    compare (FullHouse _ _) (FourOfAKind _) = LT
    compare (FullHouse _ _) _ = GT
    compare (Flush h1) (Flush h2) = compare h1 h2
    compare (Flush _) RoyalFlush = LT
    compare (Flush _) (StraightFlush _) = LT
    compare (Flush _) (FourOfAKind _) = LT
    compare (Flush _) (FullHouse _ _) = LT
    compare (Flush _) _ = GT
    compare (Straight h1) (Straight h2) = compare h1 h2
    compare (Straight _) RoyalFlush = LT
    compare (Straight _) (StraightFlush _) = LT
    compare (Straight _) (FourOfAKind _) = LT
    compare (Straight _) (FullHouse _ _) = LT
    compare (Straight _) (Flush _) = LT
    compare (Straight _) _ = GT
    compare (ThreeOfAKind t1) (ThreeOfAKind t2) = compare t1 t2
    compare (ThreeOfAKind _) (TwoPairs _ _ _) = GT
    compare (ThreeOfAKind _) (OnePair _ _) = GT
    compare (ThreeOfAKind _) (HighCard _) = GT
    compare (ThreeOfAKind _) _ = LT
    compare (TwoPairs lp1 sp1 h1) (TwoPairs lp2 sp2 h2)
        | lp1 > lp2 = GT
        | lp1 < lp2 = LT
        | sp1 > sp2 = GT
        | sp1 < sp2 = LT
        | otherwise = compare h1 h2
    compare (TwoPairs _ _ _) (OnePair _ _) = GT
    compare (TwoPairs _ _ _) (HighCard _) = GT
    compare (TwoPairs _ _ _) _ = LT
    compare (OnePair p1 h1) (OnePair p2 h2)
        | p1 > p2 = GT
        | p1 < p2 = LT
        | otherwise = compare h1 h2
    compare (OnePair _ _) (HighCard _) = GT
    compare (OnePair _ _) _ = LT
    compare (HighCard h1) (HighCard h2) = compare h1 h2
    compare (HighCard _) _ = LT


valueToInt :: Map Char Int
valueToInt = M.fromList [('A', 14), ('K', 13), ('Q', 12), ('J', 11), ('T', 10)]


readCards :: String -> [Card]
readCards line =
    let charToSuit :: Char -> Suit
        charToSuit 'S' = Spades
        charToSuit 'H' = Hearts
        charToSuit 'D' = Diamonds
        charToSuit 'C' = Clubs
        charToSuit _ = error "Unknown suit"
        getValue :: Char -> Int
        getValue r = case M.lookup r valueToInt of Nothing -> digitToInt r
                                                   Just n -> n
    in map (\[r, s] -> Card (charToSuit s) (getValue r)) $ words line


readPokerHands :: [String] -> [Game]
readPokerHands content =
    let parseLine :: String -> Game
        parseLine line =
            let (a, b) = splitAt 5 $ readCards line
            in Game (Hand a) (Hand b)
    in map parseLine content


calcRank :: Hand -> Rank
calcRank (Hand cards)
    | nrSuits == 1 =
        if values == [14, 13, 12, 11, 10] then RoyalFlush
        else if all (== 1) consecutiveDifferences then StraightFlush (head values)
        else if aceReplaced == [5, 4, 3, 2, 1] then StraightFlush 5
        else Flush (head values)
    | all (== 1) consecutiveDifferences = Straight (head values)
    | aceReplaced == [5, 4, 3, 2, 1] = Straight 5
    | otherwise = case counts of
        (v, 4) : _ -> FourOfAKind v
        [(three, 3), (two, 2)] -> FullHouse three two
        (three, 3) : _ -> ThreeOfAKind three
        [(p1, 2), (p2, 2), (h, 1)] -> TwoPairs (max p1 p2) (min p1 p2) h
        (p, 2) : rest -> OnePair p (sortOn Down $ map fst rest)
        _ -> HighCard values
    where
        values = sortOn Down $ map value cards
        aceReplaced = map (\v -> if v == 14 then 1 else v) values
        nrSuits = length $ nub $ map suit cards
        consecutiveDifferences = zipWith (-) values (tail values)
        counts = sortOn (\(_, count) -> Down count) $ toList $ fromListWith (+) $ map (, 1) values


calcFirstPlayerWins :: [Game] -> Int
calcFirstPlayerWins = foldl (\acc (Game hand1 hand2) -> if calcRank hand1 > calcRank hand2 then acc + 1 else acc) 0


main :: IO()
main = do
    content <- lines <$> readFile "../resources/P0054.dat"
    let games = readPokerHands content
    print $ calcFirstPlayerWins games
