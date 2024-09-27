module Days.Day15 (runDay) where

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
import Algorithm.Search (aStar)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    U.mapFromNestedLists <$> row `sepBy` endOfLine
    where 
        row = do
            many1 ((\c -> read [c]) <$> digit)

------------ TYPES ------------
type Input = Map (Int, Int) Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = case aStar next cost heuristic isGoal start of
    Just (cost, _) -> cost
    Nothing -> error "No path found"
    where
        next (x, y) = filter (\(x,y) -> xmin <= x && x <= xmax && ymin <= y && y <= ymax) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        cost _ (x2, y2) = input Map.! (x2, y2)
        heuristic (x, y) = abs (x - xmax) + abs (y - ymax)
        isGoal = (== (xmax, ymax))
        start = (0, 0)
        (xmin, xmax, ymin, ymax) = U.mapBoundingBox input

------------ PART B ------------
partB :: Input -> OutputB
partB input = case aStar next cost heuristic isGoal start of
    Just (cost, _) -> cost
    Nothing -> error "No path found"
    where
        next (x, y) = filter (\(x,y) -> xmin <= x && x < nx * 5 && ymin <= y && y < ny * 5) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        cost _ (x2, y2) = if baseCost > 9 then baseCost - 9 else baseCost
            where
                baseCost = input Map.! (x2 `mod` ny, y2 `mod` ny) + x2 `div` nx + y2 `div` ny
        heuristic (x, y) = abs (x - fst goal) + abs (y - snd goal)
        isGoal = (== goal)
        start = (0, 0)
        goal = (nx * 5 - 1, ny * 5 - 1)
        nx = xmax + 1
        ny = ymax + 1
        (xmin, xmax, ymin, ymax) = U.mapBoundingBox input
