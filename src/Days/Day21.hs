{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move map inside list comprehension" #-}
module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Bifunctor (first, Bifunctor (second), bimap)
import Debug.Trace (traceShowId, traceShow)
import Data.Multiset (Multiset)
import qualified Data.Multiset as MS
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (\players -> State players 0 (Dice 0 0)) <$> (player `sepBy` endOfLine)
    where
        player = do
            string "Player "
            decimal
            string " starting position: "
            Player 0 . subtract 1 <$> decimal

------------ TYPES ------------
type Input = GameState
data Player = Player { score :: Int, position :: Int } deriving (Show, Eq)
data Dice = Dice { roll :: Int, n :: Int } deriving (Show, Eq)
data GameState = State { players :: [Player], cur :: Int, die :: Dice } | GameOver { winner :: Int } deriving (Show, Eq)

instance Ord GameState where
    compare (State ps1 c1 d1) (State ps2 c2 d2) = compare (ps1, c1, d1) (ps2, c2, d2)
    compare (GameOver w1) (GameOver w2) = compare w1 w2
    compare (State {}) (GameOver _) = LT
    compare (GameOver _) (State {}) = GT

instance Ord Player where
    compare (Player s1 p1) (Player s2 p2) = compare (s1, p1) (s2, p2)

instance Ord Dice where
    compare _ _ = EQ -- don't care about number of moves happened and roll doesn't affect part 2

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- takes a position and a dice, returns the new position and the new dice
rollDice3 :: (Int, Dice) -> (Int, Dice)
rollDice3 (pos, Dice roll n) = (pos', Dice ((roll + 3) `mod` 10) (n + 3))
    where
        move = 3 * roll + 6
        pos' = (pos + move) `mod` 10

doMove :: ((Int, Dice) -> (Int, Dice)) -> GameState -> GameState
doMove rollF (State players cur die) = State players' cur' die'
    where
        curplayer = players !! cur
        (pos', die') = rollF (position curplayer, die)
        cur' = (cur + 1) `mod` length players
        score' = score curplayer + pos' + 1
        player' = curplayer { position = pos', score = score' }
        players' = Data.List.take cur players ++ [player'] ++ drop (cur + 1) players

partA :: Input -> OutputA
partA initialState = (*) <$> n . die <*> head . filter (< 1000) . map score . players $ finalState
    where finalState = head $ dropWhile (\(State ps _ _) -> not $ any ((>= 1000) . score) ps) $ iterate (doMove rollDice3) initialState

------------ PART B ------------

rollDiceN :: Int -> (Int, Dice) -> (Int, Dice)
rollDiceN n (pos, Dice roll moves) = (pos', Dice roll (moves + 1))
    where
        pos' = (pos + n) `mod` 10

-- takes a game state and returns all possible game states after a move

diracDice :: Multiset Int
diracDice = MS.fromList [r1+r2+r3 | r1 <- [1..3], r2 <- [1..3], r3 <- [1..3]]

createUniverses :: (Int, Int, Multiset GameState) -> (Int, Int, Multiset GameState)
createUniverses (w1, w2, gs) = traceShow (w1, w2, MS.size gs, MS.distinctSize gs) $ foldl update (w1, w2, MS.empty) $ MS.toGroupList gs
    where
        update (w1', w2', states) (state, count) = (w1' + count * w1'', w2' + count * w2'', MS.unionWith (+) states $ MS.mapCounts (* count) states')
            where
                (w1'', w2'', states') = branchCountWins state
        branchCountWins state = ( -- (wins for player 1, wins for player 2, new states)
                MS.count (GameOver 0) branches,
                MS.count (GameOver 1) branches,
                MS.filter (not . isGameOver) branches)
            where
                branches = branchUniverses state
        branchUniverses state = MS.map (\r -> endGame $ doMove (rollDiceN r) state) diracDice
        endGame state | score (head (players state)) >= 21 = GameOver 0
                      | score (last (players state)) >= 21 = GameOver 1
                      | otherwise = state

isGameOverP :: Int -> GameState -> Bool
isGameOverP 0 (GameOver 0) = True
isGameOverP 1 (GameOver 1) = True
isGameOverP _ _ = False

isGameOver :: GameState -> Bool
isGameOver (GameOver _) = True
isGameOver _ = False

partB :: Input -> OutputB
partB i@(State ps c d) =
    max <$> fst <*> snd $
    head $ dropWhile (not . MS.null . third) $ iterate createUniverses (0, 0, MS.singleton i)
    where
        fst (x, _, _) = x
        snd (_, x, _) = x
        third (_, _, x) = x
