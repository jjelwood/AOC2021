module Days.Day12 (runDay) where

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
import Data.Char (isAsciiUpper, isAsciiLower)
import Debug.Trace
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    Map.unionsWith Set.union <$> row `sepBy` endOfLine
    where
        row = do
            from <- many1 letter
            char '-'
            to <- many1 letter
            return $ Map.fromList [(from, Set.singleton to), (to, Set.singleton from)]

------------ TYPES ------------
type Input = Map String (Set String)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
findAllPathsA :: Map String (Set String) -> String -> String -> Set String -> Set [String]
findAllPathsA m from to visited
    | from == to = Set.singleton [to]
    | otherwise = Set.unions $
        Set.map (\n -> if n `Set.member` visited then Set.empty else Set.map (from:) $ findAllPathsA m n to visited') neighbours
        where
            visited' = if all isAsciiLower from then Set.insert from visited else visited
            neighbours = fromMaybe Set.empty $ m Map.!? from

findAllPathsB :: Map String (Set String) -> String -> String -> Map String Int -> Set [String]
findAllPathsB m from to visited
    | from == to = Set.singleton [to]
    | otherwise = Set.unions $
        Set.map (\n -> if not $ isTraversable n then Set.empty else Set.map (from:) $ findAllPathsB m n to visited') neighbours
        where
            isTraversable n | n == "start" || n == "end" = count n == 0
                            | all isAsciiLower n = count n == 0 || Map.size (Map.filterWithKey (\x c -> c > 1 && all isAsciiLower x) visited') == 0
                            | otherwise = True
            visited' = Map.insert from (count from + 1) visited
            neighbours = fromMaybe Set.empty $ m Map.!? from
            count n = Map.findWithDefault 0 n visited

partA :: Input -> OutputA
partA input = Set.size $ findAllPathsA input "start" "end" Set.empty

------------ PART B ------------
partB :: Input -> OutputB
partB input = Set.size $ findAllPathsB input "start" "end" Map.empty
