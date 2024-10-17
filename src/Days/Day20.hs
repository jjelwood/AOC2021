module Days.Day20 (runDay) where

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
    a <- algorithm
    endOfLine
    endOfLine
    i <- image
    return (a, i)
    where
        pixel = (char '.' >> return Off) <|> (char '#' >> return On)
        algorithm = do
            Vec.fromList <$> many1 pixel
        image = do
            U.mapFromNestedLists <$> (many1 pixel `sepBy` endOfLine)

------------ TYPES ------------
type Input = (Vector Pixel, Map (Int, Int) Pixel)
data Pixel = On | Off deriving (Eq)
instance Show Pixel where
    show On = "#"
    show Off = "."

type OutputA = Int

type OutputB = Int

------------ PART A ------------
step :: Vector Pixel -> (Map (Int, Int) Pixel, Int) -> (Map (Int, Int) Pixel, Int)
step alg (m, i) = (m', i+1)
    where
        m' = Map.foldlWithKey changePoint m $ addNeighbours alg m i
        changePoint acc point _ = Map.insert point (change alg m i point) acc

addNeighbours :: Vector Pixel -> Map (Int, Int) Pixel -> Int -> Map (Int, Int) Pixel
addNeighbours v m i = Map.foldlWithKey (\acc point _ -> foldl (\acc' p -> insertIfAbsent p def acc') acc $ neighbours point) m m
    where
        def = defaultPixel v i

defaultPixel :: Vector Pixel -> Int -> Pixel
defaultPixel v i | v Vec.! 0 == Off = Off
                 | even i = Off
                 | otherwise = On

insertIfAbsent :: Ord k => k -> a -> Map k a -> Map k a
insertIfAbsent = Map.insertWith (\_ old -> old)

change :: Vector Pixel -> Map (Int, Int) Pixel -> Int -> (Int, Int) -> Pixel
change v m i p = v Vec.! readNeighbours v m i p

readNeighbours :: Vector Pixel -> Map (Int, Int) Pixel -> Int -> (Int, Int) -> Int
readNeighbours v m i point = readBin $ map (\p -> Map.findWithDefault def p m) $ neighbours point
    where
        def = defaultPixel v i

readBin :: [Pixel] -> Int
readBin = foldl (\acc pixel -> if pixel == On then 2 * acc + 1 else 2 * acc) 0

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1]]

iterateN :: Int -> Input -> OutputA
iterateN n (alg, m) = Map.size $ Map.filter (== On) $ fst $ iterate (step alg) (m, 0) !! n

partA :: Input -> OutputA
partA = iterateN 2

------------ PART B ------------
partB :: Input -> OutputB
partB = iterateN 50
