module Days.Day09 (runDay) where

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
import Data.Bifunctor
import Data.Ord (comparing)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    U.mapFromNestedLists <$> (row `sepBy` endOfLine)
    where
        row = some ((\c -> read [c]) <$> digit)

------------ TYPES ------------
type Input = Map (Int, Int) Int

type OutputA = Int

type OutputB = Int

------------ PART A ------------
isLowPoint :: Map (Int, Int) Int -> (Int, Int) -> Int -> Bool
isLowPoint m (x, y) val =
    all (maybe True (> val) . (m Map.!?)) neighbours
    where
        neighbours = map (bimap (x +) (y +)) [(-1, 0), (0, -1), (1, 0), (0, 1)]

getLowPoints :: Map (Int, Int) Int -> Map (Int, Int) Int
getLowPoints m = Map.filterWithKey (isLowPoint m) m

partA :: Input -> OutputA
partA = sum . map (+1) . Map.elems . getLowPoints

------------ PART B ------------
incNeighbours :: Map (Int, Int) Int -> (Int, Int) -> Int -> [(Int, Int)]
incNeighbours m (x, y) val = filter (maybe False (\x -> x /= 9 && x > val) . (m Map.!?)) neighbours
    where
        neighbours = map (bimap (x +) (y +)) [(-1, 0), (0, -1), (1, 0), (0, 1)]

makeGraph :: Map (Int, Int) Int -> Map (Int, Int) [(Int, Int)]
makeGraph m = Map.mapWithKey (incNeighbours m) m

findComponent :: Map (Int, Int) [(Int, Int)] -> (Int, Int) -> Set (Int, Int)
findComponent m p = findComponentR m p (Set.singleton p)

findComponentR :: Map (Int, Int) [(Int, Int)] -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
findComponentR m p s = foldl f s (Set.fromList neighbours)
    where
        neighbours = m Map.! p
        f comp n = 
            if Set.member n comp 
                then comp 
                else comp `Set.union` findComponentR m n (Set.insert n comp)

partB :: Input -> OutputB
partB input = 
    product . 
    Data.List.take 3 . sortBy (comparing negate) . 
    map (Set.size . findComponent adjMap) . 
    Map.keys $ getLowPoints input
    where
        adjMap = makeGraph input
