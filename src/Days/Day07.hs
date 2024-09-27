module Days.Day07 (runDay) where

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
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
fuelCostA :: Int -> [Int] -> Int
fuelCostA x = sum . map (\c -> abs (c - x))

minCost :: (Int -> [Int] -> Int) -> [Int] -> Int
minCost f input = minimum $ map (`f` input) [minimum input .. maximum input]

partA :: Input -> OutputA
partA = minCost fuelCostA

------------ PART B ------------
fuelCostB :: Int -> [Int] -> Int
fuelCostB x = sum . map cost
    where
        dist c = abs (c - x)
        cost c = dist c * (dist c + 1) `div` 2

partB :: Input -> OutputB
partB = minCost fuelCostB
