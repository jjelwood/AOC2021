module Days.Day14 (runDay) where

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
import Debug.Trace
import Data.MemoTrie
import Data.Ord (comparing)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    initialState <- many1 letter
    endOfLine
    endOfLine
    insertions <- insertion `sepBy` endOfLine
    return (initialState, Map.fromList insertions)
    where
        insertion = do
            first <- letter
            second <- letter
            string " -> "
            insert <- letter
            return ((first, second), insert)

------------ TYPES ------------
type Input = (String, Map (Char, Char) Char)

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
makeInsertionsFold :: Map (Char, Char) Char -> String -> String
makeInsertionsFold _ [] = []
makeInsertionsFold _ [x] = [x]
makeInsertionsFold m (a:b:rest) = case m Map.!? (a, b) of
    Just c -> a : c : makeInsertionsFold m (b:rest)
    Nothing -> a : makeInsertionsFold m (b:rest)

makeInsertionsRec :: Map (Char, Char) Char -> String -> String
makeInsertionsRec _ [] = []
makeInsertionsRec _ [x] = [x]
makeInsertionsRec m [a, b] = case m Map.!? (a, b) of
    Just c -> [a, c, b]
    Nothing -> [a, b]
makeInsertionsRec m xs = case m Map.!? (a, b) of
    Just c -> makeInsertionsRec m first ++ c : makeInsertionsRec m second
    Nothing -> makeInsertionsRec m first ++ makeInsertionsRec m second
    where
        first = Data.List.take midPoint xs
        second = Data.List.drop midPoint xs
        a = last first
        b = head second
        midPoint = length xs `div` 2

makeInsertionsMemo :: Map (Char, Char) Char -> String -> String
makeInsertionsMemo = memo . makeInsertionsRec

countPairs :: String -> Map (Char, Char) Integer
countPairs [] = Map.empty
countPairs [x] = Map.empty
countPairs (a:b:rest) = Map.insertWith (+) (a, b) 1 $ countPairs (b:rest)

countChars :: String -> Map Char Integer
countChars = Map.fromListWith (+) . map (,1)

makeInsertionsPairCounts :: Map (Char, Char) Char -> (Map (Char, Char) Integer, Map Char Integer) -> (Map (Char, Char) Integer, Map Char Integer)
makeInsertionsPairCounts m (pairCounts, charCounts) = (newPairCounts, newCharCounts)
    where
        (newPairCounts, newCharCounts) = foldl (\(pc,cc) (k, v) -> (Map.unionWith (+) pc (insertPair k v), Map.unionWith (+) cc (insertChar k v))) (Map.empty, charCounts) $ Map.toList pairCounts
        insertPair (a, b) _ = Map.fromList [((a,c), count), ((c,b), count)]
            where
                count = pairCounts Map.! (a, b)
                c = m Map.! (a, b)
        insertChar (a, b) _ = Map.singleton (m Map.! (a, b)) (pairCounts Map.! (a, b))

iterateAndScore :: Int -> Input -> Integer
iterateAndScore n (initialState, insertions) = ((-) <$> maximum <*> minimum) $ map snd charCounts
    where
        charCounts = Map.toList $ snd $ (!! n) $ iterate (makeInsertionsPairCounts insertions) (countPairs initialState, countChars initialState)

partA :: Input -> OutputA
partA = iterateAndScore 10

------------ PART B ------------

partB :: Input -> OutputB
partB = iterateAndScore 40
