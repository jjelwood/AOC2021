module Days.Day05 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    line `sepBy` endOfLine
    where
        line = do
            x1 <- decimal
            char ','
            y1 <- decimal
            string " -> "
            x2 <- decimal
            char ','
            y2 <- decimal
            return ((x1, y1), (x2, y2))

------------ TYPES ------------
type Input = [Line]
type Line = ((Int, Int), (Int, Int))

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isDiagonal :: Line -> Bool
isDiagonal ((x1, y1), (x2, y2)) = x1 /= x2 && y1 /= y2

rangeRepeat :: Int -> Int -> [Int]
rangeRepeat a b
    | a < b = [a..b]
    | a == b = repeat a
    | otherwise = [a,a-1..b]

createLineMap :: Line -> Map (Int, Int) Int
createLineMap ((x1, y1), (x2, y2)) = Map.fromList $ map (,1) points
    where
        points = zip (rangeRepeat x1 x2) (rangeRepeat y1 y2) 

populate :: [Line] -> Map (Int, Int) Int
populate =
    Map.unionsWith (+) .
    map createLineMap

countOverlaps :: Map (Int, Int) Int -> Int
countOverlaps = length . Map.filter (> 1)

partA :: Input -> OutputA
partA = countOverlaps . populate . filter (not . isDiagonal)

------------ PART B ------------
partB :: Input -> OutputB
partB = countOverlaps . populate
