module Days.Day04 (runDay) where

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
import Control.Applicative ((<|>), Alternative (some))
import Data.Type.Coercion (trans)
import Control.Arrow (Arrow(first))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    numbers <- decimal `sepBy` char ','
    endOfLine
    endOfLine
    boards <- board `sepBy` endOfLine
    return (numbers, boards)
    where
        board = some rowP
        rowP = do
            row <- num `sepBy1` char ' '
            endOfLine <|> endOfInput
            return row
        num = decimal <|> numWSpace
        numWSpace = do
            char ' '
            decimal

------------ TYPES ------------
type Input = ([Int], [Board])
type Board = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

isBingo :: Set.Set Int -> Board -> Bool
isBingo played board = any (any (all (`Set.member` played))) [board, transpose board]

score :: Int -> Board -> Set.Set Int -> Int
score n board played = n * sum (concatMap (filter (`Set.notMember` played)) board)

play :: [Int] -> [Board] -> Set.Set Int -> Int
play (n:ns) bs played = case completedBoard of
    Just b -> score n b newPlayed
    Nothing -> play ns bs newPlayed
    where
        completedBoard = find (isBingo newPlayed) bs
        newPlayed = Set.insert n played

partA :: Input -> OutputA
partA input = play (fst input) (snd input) Set.empty

------------ PART B ------------
playRemoveWinners :: [Int] -> [Board] -> Set.Set Int -> Int
playRemoveWinners (n:ns) bs played
    | length newBoards == 1 = play ns newBoards newPlayed
    | otherwise = playRemoveWinners ns newBoards newPlayed
    where
        newBoards = filter (not . isBingo newPlayed) bs
        newPlayed = Set.insert n played

partB :: Input -> OutputB
partB input = playRemoveWinners (fst input) (snd input) Set.empty
