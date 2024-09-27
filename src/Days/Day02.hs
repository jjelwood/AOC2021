module Days.Day02 (runDay) where

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
import Data.Time.Format.ISO8601 (yearFormat)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  line `sepBy` endOfLine
  where
    line = do
      dir <- direction
      char ' '
      num <- decimal
      return (dir, num)
    direction = do
      choice [
        string "up" >> return Up,
        string "down" >> return Down,
        string "forward" >> return Forward]

------------ TYPES ------------
data Direction = Up | Down | Forward
  deriving (Show, Eq)

type Input = [(Direction, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
move :: (Int, Int) -> (Direction, Int) -> (Int, Int)
move (x, y) (Up, num) = (x, y - num)
move (x, y) (Down, num) = (x, y + num)
move (x, y) (Forward, num) = (x + num, y)

partA :: Input -> OutputA
partA input = ((*) <$> fst <*> snd) $ foldl move (0,0) input

------------ PART B ------------
moveAim :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
moveAim (x, y, a) (Up, num) = (x, y, a - num)
moveAim (x, y, a) (Down, num) = (x, y, a + num)
moveAim (x, y, a) (Forward, num) = (x + num, y + num * a, a)

partB :: Input -> OutputB
partB input = ((*) <$> fst3 <*> snd3) $ foldl moveAim (0, 0, 0) input
    where
      fst3 (a, _, _) = a
      snd3 (_, b, _) = b
