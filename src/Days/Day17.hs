module Days.Day17 (runDay) where

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
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    string "target area: x="
    xmin <- sdecimal
    string ".."
    xmax <- sdecimal
    string ", y="
    ymin <- sdecimal
    string ".."
    ymax <- sdecimal
    return (xmin, xmax, ymin, ymax)
    where
        sdecimal = do
            sign <- option 1 (char '-' >> return (-1))
            num <- decimal
            return (sign * num)

------------ TYPES ------------
type Input = (Int, Int, Int, Int)
data Pos = Pos { x :: Int, y :: Int, dx :: Int, dy :: Int } deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
step :: Pos -> Pos
step (Pos x y dx dy) = Pos (x + dx) (y + dy) (dx - signum dx) (dy - 1)

inZone :: (Int, Int, Int, Int) -> Pos -> Bool
inZone (xmin, xmax, ymin, ymax) (Pos x y _ _) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

zoneUnreachable :: (Int, Int, Int, Int) -> Pos -> Bool
zoneUnreachable (xmin, xmax, ymin, ymax) (Pos x y dx dy) = y < ymin || (x > xmax && dx >= 0) || (x < xmin && dx <= 0)

reachesZone :: (Int, Int, Int, Int) -> (Int, Int) -> Bool
reachesZone zone (dx, dy) = case find (\p -> inZone zone p || zoneUnreachable zone p) $ iterate step $ Pos 0 0 dx dy of
    Just p -> inZone zone p

partA :: Input -> OutputA
partA (xmin, xmax, ymin, ymax) | ymax < 0 = vertex (abs ymin - 1)
                               | ymin > 0 = vertex ymax
                               | otherwise = vertex (max (abs ymin - 1) ymax)
    where
        vertex maxV = maxV * (maxV + 1) `div` 2


------------ PART B ------------
partB :: Input -> OutputB
partB zone@(xmin, xmax, ymin, ymax) = length (filter (reachesZone zone) [(x, y) | x <- [-furthestX .. furthestX], y <- [-furthestY .. furthestY]])
    where
        furthestY = max (abs ymin) (abs ymax)
        furthestX = max (abs xmax) (abs xmin)
