module Days.Day13 (runDay) where

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
import Control.Applicative
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    points <- point `sepBy` endOfLine
    endOfLine
    endOfLine
    folds <- fold `sepBy` endOfLine
    return (Set.fromList points, folds)
    where
        point = do
            x <- decimal
            char ','
            y <- decimal
            return (x, y)
        fold = do
            string "fold along "
            d <- direction
            char '='
            Fold d <$> decimal
        direction = (string "x" >> return X) <|> (string "y" >> return Y)

------------ TYPES ------------
type Input = (Set (Int, Int), [Fold])
data Fold = Fold { dir :: Direction, val :: Int } deriving (Show, Eq)
data Direction = X | Y deriving (Show, Eq)

type OutputA = Int

type OutputB = String

------------ PART A ------------
foldPoint :: Fold -> (Int, Int) -> (Int, Int)
foldPoint (Fold X v) (x, y) = if x > v then (2 * v - x, y) else (x, y)
foldPoint (Fold Y v) (x, y) = if y > v then (x, 2 * v - y) else (x, y)

partA :: Input -> OutputA
partA (points, folds) = Set.size $ Set.map (foldPoint $ head folds) points

------------ PART B ------------
performFolds :: Input -> Set (Int, Int)
performFolds (points, folds) = foldl (\ps f -> Set.map (foldPoint f) ps) points folds

partB :: Input -> OutputB
partB = U.prettySet '#' ' ' . performFolds
