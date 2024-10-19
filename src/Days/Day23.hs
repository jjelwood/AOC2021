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
import Debug.Trace (traceShowId, traceShow, trace)
import Data.Bifunctor (second)
import GHC.IO (unsafePerformIO)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    Burrow . mapToVec Blocked . U.mapFromNestedLists . transpose
    <$> many1 (choice (map (fmap location . char) "#.ABCD ")) `sepBy` endOfLine

mapToVec :: a -> Map (Int, Int) a -> Vector a
mapToVec def m = Vec.fromList $ concatMap (\y -> map (\x -> fromMaybe def $ m Map.!? (x, y)) [1..11]) [1..3]

location :: Char -> Location
location '.' = Open
location '#' = Blocked
location ' ' = Blocked
location 'A' = Occupied A
location 'B' = Occupied B
location 'C' = Occupied C
location 'D' = Occupied D

p2i :: (Int, Int) -> Int
p2i (x, y) = (y - 1) * 11 + (x - 1)

i2p :: Int -> (Int, Int)
i2p i = (i `mod` 11 + 1, i `div` 11 + 1)

lookupP :: Burrow -> (Int, Int) -> Location
lookupP (Burrow v) (x, y) = v Vec.! p2i (x, y)

lookupI :: Burrow -> Int -> Location
lookupI (Burrow v) i = v Vec.! i

indices :: Burrow -> [Int]
indices (Burrow v) = [0..Vec.length v - 1]

------------ TYPES ------------
type Input = Burrow
data Location = Open | Blocked | Occupied Amphipod deriving (Eq, Show)
data Amphipod = A | B | C | D deriving (Eq, Show)
newtype Burrow = Burrow { locations :: Vector Location } deriving (Eq, Show)

printBurrowA :: Burrow -> IO ()
printBurrowA b = putStrLn $ unlines $ topLine : middleLines ++ [bottomLine]
    where
        topLine = "#############"
        middleLines = [ "#" ++ concatMap (showLocation . lookupI b) [p2i (x, y) | x <- [1..11]] ++ "#" | y <- [1] ]
                                ++ [ "###" ++ intercalate "#" [showLocation (lookupI b (p2i (x, y))) | x <- [3, 5, 7, 9]] ++ "###" | y <- [2] ]
                                ++ [ "  #" ++ intercalate "#" [showLocation (lookupI b (p2i (x, y))) | x <- [3, 5, 7, 9]] ++ "#" | y <- [3] ]
        bottomLine = "  #########"

printBurrowB :: Burrow -> IO ()
printBurrowB b = putStrLn $ unlines $ topLine : middleLines ++ [bottomLine]
    where
        topLine = "#############"
        middleLines = [ "#" ++ concatMap (showLocation . lookupI b) [p2i (x, y) | x <- [1..11]] ++ "#" | y <- [1] ]
                                ++ [ "###" ++ intercalate "#" [showLocation (lookupI b (p2i (x, y))) | x <- [3, 5, 7, 9]] ++ "###" | y <- [2] ]
                                ++ [ "  #" ++ intercalate "#" [showLocation (lookupI b (p2i (x, y))) | x <- [3, 5, 7, 9]] ++ "#" | y <- [3] ]
                                ++ [ "  #" ++ intercalate "#" [showLocation (lookupI b (p2i (x, y))) | x <- [3, 5, 7, 9]] ++ "#" | y <- [4] ]
                                ++ [ "  #" ++ intercalate "#" [showLocation (lookupI b (p2i (x, y))) | x <- [3, 5, 7, 9]] ++ "#" | y <- [5] ]
        bottomLine = "  #########"

showLocation :: Location -> String
showLocation Open = "."
showLocation Blocked = "#"
showLocation (Occupied A) = "A"
showLocation (Occupied B) = "B"
showLocation (Occupied C) = "C"
showLocation (Occupied D) = "D"

instance Ord Burrow where
    compare (Burrow v1) (Burrow v2) = compare v1 v2

instance Ord Location where
    compare Open Open = EQ
    compare (Occupied a) (Occupied b) = compare a b
    compare Open _ = LT
    compare (Occupied _) _ = GT
    compare Blocked Blocked = EQ
    compare Blocked _ = GT

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

rooms :: Int -> Map Amphipod [Int]
rooms depth = Map.fromList $
    map (second (map p2i))
    [ (A, [(3, 1 + i) | i <- [1..depth]])
    , (B, [(5, 1 + i) | i <- [1..depth]])
    , (C, [(7, 1 + i) | i <- [1..depth]])
    , (D, [(9, 1 + i) | i <- [1..depth]])
    ]

allRooms :: Int -> [Int]
allRooms depth = concat $ Map.elems $ rooms depth

isOutsideRoom :: Int -> Bool
isOutsideRoom i = y == 1 && x `elem` [3, 5, 7, 9]
    where (x, y) = i2p i

isInHallway :: Int -> Bool
isInHallway i = y == 1
    where (x, y) = i2p i

roomHasNoInvalids :: Int -> Burrow -> Amphipod -> Bool
roomHasNoInvalids depth b a = all (\i -> lookupI b i == Occupied a || lookupI b i == Open) $ rooms depth Map.! a

openNeighbours :: Burrow -> Int -> [Int]
openNeighbours b@(Burrow v) i = filter (\j -> j >= 0 && j < Vec.length v && lookupI b j == Open) $
    map p2i [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    where (x, y) = i2p i

allNextIndices :: Burrow -> Int -> [(Int, Int)]
allNextIndices b i = filter (\pair -> not (pair == (i, 0) || isOutsideRoom (fst pair))) $
        dfs b Set.empty [(i, 0)]
    where
        dfs _ _ [] = []
        dfs b visited ((current, dist):stack)
            | current `Set.member` visited = dfs b visited stack
            | otherwise = (current, dist) : dfs b (Set.insert current visited) (foldl (\acc n -> n : acc) stack next)
            where
                next = map (, dist + 1) $ openNeighbours b current

nextStatesCost :: Int -> Burrow -> [(Burrow, Int)]
nextStatesCost depth b@(Burrow v) = foldl (\acc i -> acc ++ movesFrom depth b i) [] $ indices b

movesFrom :: Int -> Burrow -> Int -> [(Burrow, Int)]
movesFrom depth b i
    | lookupI b i == Open || lookupI b i == Blocked = []
    | otherwise =
        map (makeMove b i) $
        filter (\(j, _) -> not (isInHallway i && isInHallway j)) $ -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway
         -- Don't move from hallway to hallway
         -- Don't move from hallway to hallway
         -- Don't move from hallway to hallway
         -- Don't move from hallway to hallway
         -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway -- Don't move from hallway to hallway
         -- Don't move from hallway to hallway
        filter (\(j, _) -> isInHallway j || (j `elem` rooms depth Map.! a && roomHasNoInvalids depth b a)) $ -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid -- Don't move into a room if it's not valid
        allNextIndices b i
        where a = amphipodType b i

makeMove :: Burrow -> Int -> (Int, Int) -> (Burrow, Int)
makeMove b@(Burrow v) fromI (toI, steps) = (Burrow (v Vec.// [(fromI, Open), (toI, Occupied a)]), steps * moveCost a)
    where a = amphipodType b fromI

amphipodType :: Burrow -> Int -> Amphipod
amphipodType b i = case lookupI b i of Occupied a -> a; _ -> error "Not an amphipod"

hCost :: Burrow -> Int
hCost b = foldl (\acc i -> acc + cost i) 0 $ indices b
    where cost i | lookupI b i == Open || lookupI b i == Blocked = 0
                 | otherwise = moveCost (amphipodType b i)

dist :: Int -> Int -> Amphipod -> Int
dist depth i a | i `elem` rooms depth Map.! a = 0
              | otherwise = moveCost a * (abs (x - goalX) + abs (y - goalY) + if x /= goalX then abs (y - 1) else 0)
    where
        (x, y) = i2p i
        (goalX, goalY) = i2p $ rooms depth Map.! a !! 1 -- The first room is the goal as it's the furthest away, so always underestimates

isSolution :: Int -> Burrow -> Bool
isSolution depth b = Map.foldlWithKey (\acc a ps -> acc && all (\p -> lookupI b p == Occupied a) ps) True $ rooms depth -- All rooms are occupied by the correct amphipod

solve :: Int -> Burrow -> Maybe (Int, [Burrow])
solve depth = aStarAssoc (nextStatesCost depth) hCost (isSolution depth)

-- partA :: Input -> OutputA
-- partA start = case solve 2 start of
--     Just (cost, path) -> unsafePerformIO (mapM_ printBurrowA path) `seq` cost
--     Nothing -> error "No path found"

partA :: Input -> OutputA
partA start = -1

------------ PART B ------------
extraRows :: Vector Location
extraRows = Vec.replicate 22 Blocked Vec.//
    [(2, Occupied D), (4, Occupied C), (6, Occupied B), (8, Occupied A),
     (2 + 11, Occupied D), (4 + 11, Occupied B), (6 + 11, Occupied A), (8 + 11, Occupied C)]

insertRows :: Burrow -> Burrow
insertRows (Burrow v) = Burrow $ Vec.concat [Vec.slice 0 22 v, extraRows, Vec.slice 22 11 v]

partB :: Input -> OutputB
partB inp = case solve 4 (insertRows inp) of
    Just (cost, path) -> unsafePerformIO (mapM_ printBurrowB path) `seq` cost
    Nothing -> error "No path found"

-- partB :: Input -> OutputB
-- partB inp = unsafePerformIO (printBurrowB (insertRows inp)) `seq` 0
