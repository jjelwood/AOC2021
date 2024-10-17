module Days.Day22 (runDay) where

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
import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = cuboid `sepBy` endOfLine
    where
        cuboid = do
            cube <- On <$ string "on " <|> Off <$ string "off "
            ranges <- range `sepBy` char ','
            return $ Cuboid cube ranges
        range = do
            anyChar
            char '='
            min <- signedInt
            string ".."
            max <- signedInt
            return (min, max)
        signedInt = do
            sign <- option 1 (char '-' >> return (-1))
            num <- decimal
            return (sign * num)

------------ TYPES ------------
type Input = [Cuboid]
data Cuboid = Cuboid { value :: Cube, ranges :: [(Int, Int)] } deriving (Show, Eq)
data Cube = On | Off deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
doStep :: Map (Int, Int, Int) Cube -> Cuboid -> Map (Int, Int, Int) Cube
doStep m (Cuboid cube ranges) = foldl' (\acc (x, y, z) -> Map.insert (x, y, z) cube acc) m $ rangesToList ranges
    where
        rangesToList [(xmin, xmax), (ymin, ymax), (zmin, zmax)] = [(x, y, z) | x <- [xmin..xmax], y <- [ymin..ymax], z <- [zmin..zmax]]

compress :: (Int, Int) -> Cuboid -> Cuboid
compress (rmin, rmax) (Cuboid c ranges) = Cuboid c $ map (bimap (max rmin) (min rmax)) ranges

partA :: Input -> OutputA
partA cuboids =
    length $ Map.filter (== On) $ -- count on cubes
    foldl' doStep Map.empty -- add all cuboids
    $ map (compress (-50,50)) cuboids

------------ PART B ------------
partB :: Input -> OutputB
partB cuboids = 
    length $ Map.filter (== On) $ -- count on cubes
    foldl' doStep Map.empty cuboids
