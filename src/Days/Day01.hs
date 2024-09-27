module Days.Day01 (runDay) where

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
  sepBy1 decimal endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

countIncreasesWindow :: Int -> [Int] -> Int
countIncreasesWindow _ [] = 0
countIncreasesWindow n xs
  | length xs <= n = 0
  | otherwise = (if sum (Data.List.take n xs) < sum (Data.List.take n $ tail xs) then 1 else 0) 
    + countIncreasesWindow n (tail xs)

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)

------------ PART A ------------
partA :: Input -> OutputA
partA = countIncreases

------------ PART B ------------
partB :: Input -> OutputB
partB = countIncreasesWindow 3
