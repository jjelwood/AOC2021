module Days.Day11 (runDay) where

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
    U.mapFromNestedLists <$> (row `sepBy` endOfLine)
    where
        row = some ((\c -> read [c] :: Int) <$> digit)

------------ TYPES ------------
type Input = Map (Int, Int) Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

flash :: Map (Int, Int) Int -> (Int, Int) -> Set (Int, Int) -> (Map (Int, Int) Int, Set (Int, Int))
flash m point flashed = foldl inc (m, flashed) (neighbours point)
    where
        inc (acc, flashed) p
            | not $ p `Map.member` acc
                = (acc, flashed)
            | acc Map.! p >= 9 && not (p `Set.member` flashed)
                = flash acc' p (Set.insert p flashed)
            | otherwise
                = (acc', flashed)
            where
                acc' = Map.adjust (+1) p acc

step :: (Map (Int, Int) Int, Int, Set (Int, Int)) -> (Map (Int, Int) Int, Int, Set (Int, Int))
step (m, n, f) = (fst removedFlashed, snd removedFlashed, flashed)
    where
        removedFlashed = foldl (\(m', count) p -> (Map.insert p 0 m', count + 1)) (flashMap, n) flashed
        (flashMap, flashed) = Map.foldlWithKey (\acc p v -> if v > 9 && not (p `Set.member` snd acc) then flash (fst acc) p (p `Set.insert` snd acc) else acc) (incMap, Set.empty) incMap
        incMap = Map.map (+1) m

partA :: Input -> OutputA
partA inp = snd . (!! 100) $ iterate step (inp, 0, Set.empty)
    where
        snd (_, b, _) = b

------------ PART B ------------
partB :: Input -> OutputB
partB inp = length $ Data.List.takeWhile (\(m, c, f) -> Set.size f /= inpSize) $ iterate step (inp, 0, Set.empty)
    where
        inpSize = Map.size inp
