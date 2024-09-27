{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Days.Day08 (runDay) where

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
import Debug.Trace (traceShowId)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    line `sepBy` endOfLine
    where
        signal = some (choice $ map char ['a'..'g'])
        signals = signal `sepBy` char ' '
        line = do
            ss <- signals
            string " | "
            output <- signals
            return (ss, output)

------------ TYPES ------------
type Input = [Display]
type Display = (Signals, Output)
type Signals = [String]
type Output = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . concatMap (filter (\x -> x == 2 || x == 4 || x == 3 || x == 7) . map length . snd)

------------ PART B ------------

segments :: Int -> Set Int
segments 0 = Set.fromList [0,1,2,4,5,6]
segments 1 = Set.fromList [2,5]
segments 2 = Set.fromList [0,2,3,4,6]
segments 3 = Set.fromList [0,2,3,5,6]
segments 4 = Set.fromList [1,2,3,5]
segments 5 = Set.fromList [0,1,3,5,6]
segments 6 = Set.fromList [0,1,3,4,5,6]
segments 7 = Set.fromList [0,2,5]
segments 8 = Set.fromList [0..6]
segments 9 = Set.fromList [0,1,2,3,5,6]

allSegments :: Map Int (Set Int)
allSegments = Map.fromList $ [(i, segments i) | i <- [0..9]]

lengthToSegments :: (Set Int -> Set Int -> Set Int) -> Map Int (Set Int)
lengthToSegments f = Map.unionsWith f $ map (\s -> Map.singleton (length s) s) $ Map.elems allSegments

emptyKnownSegments :: Map Int (Set Char)
emptyKnownSegments = Map.fromList $ map (\i -> (i, Set.fromList ['a'..'g'])) [0..6]

updateKnownSegments :: Map Int (Set Char) -> String -> Map Int (Set Char)
updateKnownSegments knownSegments signal =
    Map.mapWithKey adjustWires knownSegments
    where
        charSet = Set.fromList signal
        viableSegmentsU = lengthToSegments Set.union Map.! length signal
        viableSegmentsI = lengthToSegments Set.intersection Map.! length signal
        adjustWires seg wires
            | Set.member seg viableSegmentsI = Set.intersection wires charSet
            | not $ Set.member seg viableSegmentsU = Set.difference wires charSet
            | otherwise = wires

processSignals :: [String] -> Map Int (Set Char)
processSignals = foldl' updateKnownSegments emptyKnownSegments

simplify :: Map Int (Set Char) -> Map Int (Set Char)
simplify knownSegments = Map.foldlWithKey removeIfSingleton knownSegments knownSegments
    where
        removeIfSingleton m k v = 
            if Set.size v == 1
                then Map.mapWithKey (\k' v' -> if k /= k' then Set.difference v' v else v') m 
                else m

calculateNumber :: Map Char Int -> [Char] -> Int
calculateNumber m cs =  case find ((wires == ). snd) $ Map.toList allSegments of
    Just (n,s) -> n
    Nothing -> error "No match found"
    where
        wires = Set.fromList $ map (m Map.!) cs

-- calculate :: Map Int (Set Char) -> String -> Int
-- calculate knownSegments signal =

invertMap :: (Ord a, Ord b) => Map a b -> Map b a
invertMap = Map.fromList . map (\(a,b) -> (b,a)) . Map.toList

calculateSignals :: Signals -> Map Char Int
calculateSignals = invertMap . Map.map (head . Set.toList) . simplify . processSignals

compute :: Display -> Int
compute (signals, output) = foldl (\acc x -> acc * 10 + calculateNumber wireMap x) 0 output
    where
        wireMap = calculateSignals signals

partB :: Input -> OutputB
partB = sum . map compute
