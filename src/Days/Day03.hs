module Days.Day03 (runDay) where

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
    lines <- line `sepBy` endOfLine
    return $ map (map charToInt) lines
    where
        line = many' (choice [char '0', char '1'])
        charToInt '0' = 0
        charToInt '1' = 1
        charToInt _ = error "Unexpected character"

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

compareSizes :: [Int] -> [Int] -> Ordering
compareSizes xs ys = case compare (length xs) (length ys) of
    EQ -> compare (head xs) (head ys)
    result -> result

findMostCommon :: [Int] -> Int
findMostCommon = head . maximumBy compareSizes . group . sort

findLeastCommon :: [Int] -> Int
findLeastCommon = head . minimumBy compareSizes . group . sort

selectFromColumn :: ([Int] -> Int) -> [[Int]]  -> [Int]
selectFromColumn selector = map selector . transpose

convertBinaryToDecimal :: [Int] -> Int
convertBinaryToDecimal = foldl (\acc x -> acc * 2 + x) 0

partA :: Input -> OutputA
partA input = product $ 
    map (\f -> convertBinaryToDecimal $ selectFromColumn f input) [findMostCommon, findLeastCommon]

------------ PART B ------------
bitCriteriaSearch :: ([Int] -> Int) -> Int -> [[Int]] -> [Int]
bitCriteriaSearch f n input 
    | length validRows == 1 = head validRows
    | otherwise = bitCriteriaSearch f (n + 1) validRows
    where
        validBits = selectFromColumn f input
        validRows = filter (\row -> row !! n == validBits !! n) input

partB :: Input -> OutputB
partB input = product $ 
    map (\f -> convertBinaryToDecimal $ bitCriteriaSearch f 0 input) [findMostCommon, findLeastCommon]
