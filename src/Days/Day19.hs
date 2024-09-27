{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Days.Day19 (runDay) where

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
import Data.Matrix
import qualified Data.Bifunctor
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    scanner `sepBy` (endOfLine >> endOfLine)
    where
        scanner = do
            string "--- scanner "
            n <- decimal
            string " ---"
            endOfLine
            Scanner n <$> line `sepBy` endOfLine
            where
                line = do
                    xs <- sdecimal `sepBy1` char ','
                    return $ colVector $ Vec.fromList xs
                sdecimal = do
                    sign <- option 1 (char '-' >> return (-1))
                    num <- decimal
                    return (sign * num)


------------ TYPES ------------
type Input = [Scanner]
data Scanner = Scanner { num :: Int, offsets :: [Matrix Int] } deriving (Eq, Show)

instance Ord a => Ord (Matrix a) where
    compare a b = compare (toList a) (toList b)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

fingerprints :: Scanner -> Map (Int, Int, Int) (Matrix Int, Matrix Int)
fingerprints b = Map.fromList $ map (\pair -> ((dist pair, minDisp pair, maxDisp pair), pair)) $ pairs (offsets b)
    where
        dist = sum . disps
        minDisp = minimum . disps
        maxDisp = maximum . disps

disps :: Num a => (Matrix a, Matrix a) -> Matrix a
disps (p1, p2) = abs <$> p1 - p2

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

dirs :: Num a => Matrix a -> [Matrix a]
dirs p = map (`negs` p) $ subsets [0,1,2]
    where
        neg = mapCol (\_ x -> -x)
        negs cols = foldl (.) id $ map neg cols

permMatrices :: Int -> [Matrix Int]
permMatrices n = map makeMatrix $ Data.List.permutations [1..n]
    where makeMatrix perm = matrix n n (\(i, j) -> if perm !! (i - 1) == j then 1 else 0)

configurations :: Int -> [Matrix Int]
configurations = nub . concatMap dirs . permMatrices

findOverlappingRegions :: [Scanner] -> [(Scanner, Scanner)]
findOverlappingRegions bs = [(bs !! i, bs !! j) |
                               i <- [0 .. length bs - 1],
                               j <- [i+1 .. length bs - 1],
                               i /= j,
                               12 <= Set.size (Map.keysSet (fingerprints (bs !! i)) `Set.intersection` Map.keysSet (fingerprints (bs !! j)))]

-- trys to find the relative location of the first scanner with respect to the second scanner
triangulate :: (Scanner, Scanner) -> Maybe (Matrix Int)
triangulate (s1, s2) = if Map.size overlappingFingerprints >= cn * (cn - 1) `div` 2
    then Just $ head $ Set.elems $ foldl1 Set.intersection $ map findOther $ configurations dim
    else Nothing
    where
        findOther c = foldl1 Set.intersection $ Map.elems $ Map.map (tryFindPoint c) overlappingFingerprints
        consistentValue xs = if allEqualNotNothing xs then head xs else Nothing
        overlappingFingerprints = Map.intersectionWith (,) fingerprints1 fingerprints2
        fingerprints1 = fingerprints s1
        fingerprints2 = fingerprints s2
        dim = nrows $ head $ offsets s1
        cn = length $ configurations dim

allEqualNotNothing :: Eq a => [Maybe a] -> Bool
allEqualNotNothing [] = True
allEqualNotNothing (Nothing:_) = False
allEqualNotNothing (Just x:xs) = all (== Just x) xs

tryFindPoint :: Matrix Int -> ((Matrix Int, Matrix Int), (Matrix Int, Matrix Int)) -> Set (Matrix Int)
tryFindPoint c ((p1, p2), (p3, p4)) = Set.fromList $ catMaybes [c1, c2]
  where
    c1 = if p1 + multStd c p3 == p2 + multStd c p4 then Just $ p1 + multStd c p3 else Nothing
    c2 = if p1 + multStd c p4 == p2 + multStd c p3 then Just $ p1 + multStd c p4 else Nothing

partA :: Input -> OutputA
-- partA = error "Not implemented yet!"
partA = length . traceShowId . mapMaybe triangulate . findOverlappingRegions
-- partA inp = find ((== 0) . fst) . map (Data.Bifunctor.bimap num num) $ findOverlappingRegions inp

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
