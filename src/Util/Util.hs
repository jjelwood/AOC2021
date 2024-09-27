module Util.Util where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.List (transpose)
import Data.Set (Set)
import qualified Data.Set as Set
{- ORMOLU_ENABLE -}

{-
This module contains a series of miscellaneous utility functions that I have found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
  where
    attachCoords _ _ [] = []
    attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
    attachCoords x y ((l : ls) : lss) = ((x, y), l) : (attachCoords x (y + 1) (ls : lss))

-- Splits a list into chunks of the specified size.
-- The final chunk may be smaller than the chunk size.
-- Chunk size must be positive.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n ls
  | n <= 0 = error "Cannot split into chunks of negative length."
  | null ls = []
  | length ls < n = [ls]
  | otherwise = (take n ls) : (chunksOf n (drop n ls))

-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
  | null ls = []
  | otherwise =
    let (prefix, rest) = span p ls
     in if null prefix
          then (chunksByPredicate p $ dropWhile (not . p) rest)
          else prefix : (chunksByPredicate p $ dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if
      | index < 0 -> Nothing
      | index >= (length list) -> Nothing
      | otherwise -> Just $ list !! index

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
mapBoundingBox :: Map (Int, Int) a -> (Int, Int, Int, Int)
mapBoundingBox m =
  (,,,)
    (minimum . fmap fst . Map.keys $ m)
    (maximum . fmap fst . Map.keys $ m)
    (minimum . fmap snd . Map.keys $ m)
    (maximum . fmap snd . Map.keys $ m)

setBoundingBox :: Set (Int, Int) -> (Int, Int, Int, Int)
setBoundingBox s =
  (,,,)
    (minimum . fmap fst . Set.toList $ s)
    (maximum . fmap fst . Set.toList $ s)
    (minimum . fmap snd . Set.toList $ s)
    (maximum . fmap snd . Set.toList $ s)

prettyMap :: (Show a) => Map (Int, Int) a -> String
prettyMap m = unlines $ [ [ head $ show $ m Map.! (x, y) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
  where
    (minX, maxX, minY, maxY) = mapBoundingBox m

prettySet :: Char -> Char -> Set (Int, Int) -> String
prettySet cIn cOut s = unlines $ [ [ if (x, y) `Set.member` s then cIn else cOut | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
  where
    (minX, maxX, minY, maxY) = setBoundingBox s
