module Days.Day23 (runDay) where

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
import Data.Attoparsec.Text hiding (D)
import Algorithm.Search (aStar, aStarAssoc, dijkstraAssoc)
import Data.Void
import Debug.Trace (traceShowId, traceShow)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    Map.map location . Map.filter (\c -> c /= ' ' && c /= '#') . U.mapFromNestedLists . transpose
    <$> many1 (choice (map char "#.ABCD ")) `sepBy` endOfLine

location :: Char -> Location
location '.' = Open
location 'A' = Occupied A
location 'B' = Occupied B
location 'C' = Occupied C
location 'D' = Occupied D

------------ TYPES ------------
type Input = Map (Int, Int) Location
data Location = Open | Occupied Amphipod deriving (Eq, Show)
data Amphipod = A | B | C | D deriving (Eq, Show)
data Burrow = Burrow { locations :: Map (Int, Int) Location, lastMoved :: Maybe ((Int, Int), Amphipod) } deriving (Eq, Show)

instance Ord Burrow where
    compare (Burrow l1 m1) (Burrow l2 m2) = compare (m1, l1) (m2, l2)

repBurrow :: Burrow -> [[Char]]
repBurrow (Burrow m _) = map (\y -> map (\x -> case m Map.!? (x, y) of
    Just Open -> '.'
    Just (Occupied A) -> 'A'
    Just (Occupied B) -> 'B'
    Just (Occupied C) -> 'C'
    Just (Occupied D) -> 'D'
    Nothing -> ' ') [0..12]) [1..3]

instance Ord Location where
    compare Open Open = EQ
    compare (Occupied a) (Occupied b) = compare a b
    compare Open _ = LT
    compare (Occupied _) _ = GT

instance Ord Amphipod where
    compare a1 a2 | a1 == a2 = EQ
    compare A _ = LT
    compare B A = GT
    compare B _ = LT
    compare C A = GT
    compare C B = GT
    compare C _ = LT
    compare D _ = GT

type OutputA = Int

type OutputB = Int

------------ PART A ------------
moveCost :: Amphipod -> Int
moveCost A = 1
moveCost B = 10
moveCost C = 100
moveCost D = 1000

rooms :: Map Amphipod [(Int, Int)]
rooms = Map.fromList
    [ (A, [(3, 2), (3, 3)])
    , (B, [(5, 2), (5, 3)])
    , (C, [(7, 2), (7, 3)])
    , (D, [(9, 2), (9, 3)])
    ]

allRooms :: [(Int, Int)]
allRooms = concat $ Map.elems rooms

isOutsideRoom :: Burrow -> (Int, Int) -> Bool
isOutsideRoom (Burrow m _) (x, y) = y == 1 && (x, y + 1) `elem` allRooms

isInHallway :: Burrow -> (Int, Int) -> Bool
isInHallway (Burrow m _) (x, y) = y == 1

openNeighbours :: Burrow -> (Int, Int) -> [(Int, Int)]
openNeighbours (Burrow m _) (x, y) = filter (\p -> Map.lookup p m == Just Open)
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

nextStatesCost :: Burrow -> [(Burrow, Int)]
nextStatesCost b@(Burrow m _) = Map.foldlWithKey (\acc p _ -> acc ++ movesFrom b p) [] m

movesFrom :: Burrow -> (Int, Int) -> [(Burrow, Int)]
movesFrom b@(Burrow m (Just (l, a))) p
    | m Map.! p == Open = []
    | isOutsideRoom b l && p /= l = [] -- If the last move was over a room, only that position can move
    | isInHallway b p && p /= l && m Map.! head (rooms Map.! amphipodType b p) /= Open = [] -- If the point in a hallway and point was not last to move, that position can't move if the room is occupied
    | otherwise = map (makeMove b p) $ openNeighbours b p
movesFrom b@(Burrow m Nothing) p
    | m Map.! p == Open = []
    | otherwise = map (makeMove b p) $ openNeighbours b p

makeMove :: Burrow -> (Int, Int) -> (Int, Int) -> (Burrow, Int)
makeMove b@(Burrow m _) fromP toP = (Burrow (Map.insert toP (Occupied a) $ Map.insert fromP Open m) (Just (toP, a)), moveCost a)
    where a = amphipodType b fromP

amphipodType :: Burrow -> (Int, Int) -> Amphipod
amphipodType (Burrow m _) p = case m Map.! p of Occupied a -> a; _ -> error "Not an amphipod"

hCost :: Burrow -> Int
hCost b = Map.foldlWithKey (\acc p l -> acc + dist p (amphipodType b p)) 0 $ Map.filter (/= Open) $ locations b

dist :: (Int, Int) -> Amphipod -> Int
dist (x, y) a | (x, y) `elem` rooms Map.! a = 0
              | otherwise = moveCost a * (abs (x - goalX) + abs (y - goalY) + if x /= goalX then abs (y - 1) else 0)
    where (goalX, goalY) = rooms Map.! a !! 1 -- The first room is the goal as it's the furthest away, so always underestimates

isSolution :: Burrow -> Bool
isSolution (Burrow m _) = Map.foldlWithKey (\acc a ps -> acc && all (\p -> m Map.! p == Occupied a) ps) True rooms -- All rooms are occupied by the correct amphipod

-- partA :: Input -> OutputA
-- partA start = case aStarAssoc nextStatesCost hCost isSolution (Burrow start Nothing) of
--     Just (cost, _) -> cost
--     Nothing -> error "No path found"

partA :: Input -> OutputA
partA start = case dijkstraAssoc nextStatesCost isSolution (Burrow start Nothing) of
    Just (cost, _) -> cost
    Nothing -> error "No path found"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
