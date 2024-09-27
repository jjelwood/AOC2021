module Days.Day18 (runDay) where

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
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
    snailfish `sepBy` endOfLine

snailfish :: Parser Snailfish
snailfish = snaillist <|> snailInt
    where
        snaillist = do
            char '['
            a <- snailfish
            char ','
            b <- snailfish
            char ']'
            return $ SnailList (a, b)
        snailInt = SnailInt <$> decimal

------------ TYPES ------------
type Input = [Snailfish]
data Snailfish = SnailList (Snailfish, Snailfish) | SnailInt Int deriving (Eq)
data Dir = L | R deriving (Show, Eq)

instance Show Snailfish where
    show (SnailInt x) = show x
    show (SnailList (a, b)) = "[" ++ show a ++ ", " ++ show b ++ "]"

type OutputA = Int

type OutputB = Int

------------ PART A ------------

opp :: Dir -> Dir
opp L = R
opp R = L

magnitude :: Snailfish -> Int
magnitude (SnailInt x) = x
magnitude (SnailList (a, b)) = 3 * magnitude a + 2 * magnitude b

add :: Snailfish -> Snailfish -> Snailfish
add a b = reduce $ SnailList (a, b)

reduce :: Snailfish -> Snailfish
reduce s | exploded = reduce s'
         | splited = reduce s''
         | otherwise = s
    where
        (s', exploded) = explode s
        (s'', splited) = split s'

explode :: Snailfish -> (Snailfish, Bool)
explode snail = case findExplode 0 snail of
    Just (path, l, r) -> (addToDir R path r $
                         addToDir L path l $
                         replaceAt snail (reverse path) 0, True)
    Nothing -> (snail, False)

replaceAt :: Snailfish -> [Dir] -> Int -> Snailfish
replaceAt snail [] val = SnailInt val
replaceAt (SnailList (a, b)) (L:ds) val = SnailList (replaceAt a ds val, b)
replaceAt (SnailList (a, b)) (R:ds) val = SnailList (a, replaceAt b ds val)

addToDir :: Dir -> [Dir] -> Int -> Snailfish -> Snailfish
addToDir dir path val snail = case getPathToBranch dir path of
    [] -> snail
    branchPath -> addToDirmostLeaf snail branchPath dir val

getPathToBranch :: Dir -> [Dir] -> [Dir]
getPathToBranch dir p = case branchP of
    [] -> []
    _ -> reverse (dir : tail branchP)
    where
        branchP = dropWhile (== dir) p

addToDirmostLeaf :: Snailfish -> [Dir] -> Dir -> Int -> Snailfish
addToDirmostLeaf (SnailInt x) [] _ val = SnailInt $ x + val
addToDirmostLeaf (SnailList (a, b)) [] L val = SnailList (a, addToDirmostLeaf b [] L val)
addToDirmostLeaf (SnailList (a, b)) [] R val = SnailList (addToDirmostLeaf a [] R val, b)
addToDirmostLeaf (SnailList (a, b)) (L:ds) dir val = SnailList (addToDirmostLeaf a ds dir val, b)
addToDirmostLeaf (SnailList (a, b)) (R:ds) dir val = SnailList (a, addToDirmostLeaf b ds dir val)

findExplode :: Int -> Snailfish -> Maybe ([Dir], Int, Int)
findExplode depth (SnailInt a) = Nothing
findExplode depth (SnailList (a, b))
    | depth >= 4 = case (a, b) of
        (SnailInt x, SnailInt y) -> Just ([], x, y)
        _ -> error "Invalid"
    | otherwise = addToPath L (findExplode (depth + 1) a) <|> addToPath R (findExplode (depth + 1) b)
        where addToPath dir = fmap (\(p, l, r) -> (p ++ [dir], l, r))

split :: Snailfish -> (Snailfish, Bool)
split (SnailInt a) = if a >= 10 then let val = fromIntegral a in (SnailList (SnailInt $ floor (val / 2), SnailInt $ ceiling (val / 2)), True) else (SnailInt a, False)
split (SnailList (a, b)) = (SnailList (a', if not aSplit then b' else b), aSplit || bSplit)
    where
        (a', aSplit) = split a
        (b', bSplit) = split b

partA :: Input -> OutputA
partA = magnitude . foldl1 add

------------ PART B ------------
partB :: Input -> OutputB
partB ss = maximum $ [magnitude ((ss !! i) `add` (ss !! j)) | i <- [0 .. length ss - 1], j <- [0 .. length ss - 1]]
