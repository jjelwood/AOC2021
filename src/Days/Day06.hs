module Days.Day06 (runDay) where

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
import Data.MemoTrie
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
reproduce :: Int -> Int -> Int
reproduce day timer
    | day <= 0      = 1
    | timer == 0    = reproduceNextDay 6 + reproduceNextDay 8
    | otherwise     = reproduce (day - timer) 0
    where
        reproduceNextDay = reproduce (day - 1)

reproduceMemo :: Int -> Int -> Int
reproduceMemo = memo2 reproduce

solveRecursion :: Int -> [Int] -> Int 
solveRecursion n = sum . map (reproduceMemo n)

nextDay :: Vector Int -> Vector Int
nextDay crabs = crabs 
    Vec.// [(i, newNumber i) | i <- [0..8]]
    where
        newNumber 8 = crabs Vec.! 0 
        newNumber 6 = crabs Vec.! 7 + crabs Vec.! 0
        newNumber i = crabs Vec.! (i + 1) 

getCounts :: [Int] -> Vector Int
getCounts [] = Vec.replicate 9 0
getCounts (x:xs) = rest Vec.// [(x, rest Vec.! x + 1)]
    where rest = getCounts xs

solveVector :: Int -> [Int] -> Int
solveVector n = sum . (!! n) . iterate nextDay  . getCounts

partA :: Input -> OutputA
partA = solveVector 80

------------ PART B ------------

partB :: Input -> OutputB
partB = solveVector 256
