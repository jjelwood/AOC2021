module Days.Day10 (runDay) where

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
    some (choice $ map char "()[]{}<>") `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

isOpen :: Char -> Bool
isOpen c = c `elem` ("([{<" :: String)

closes :: Char -> Char
closes '(' = ')'
closes '[' = ']'
closes '{' = '}'
closes '<' = '>'

scoreA :: Char -> Int
scoreA ')' = 3
scoreA ']' = 57
scoreA '}' = 1197
scoreA '>' = 25137

------------ PART A ------------
validate :: (Char -> Int) -> String -> ([Char], Maybe Int)
validate score = foldl f ([],Nothing)
    where
        f (stack, s) c
          | isJust s = (stack,s)
          | isOpen c = (c : stack,s)
          | null stack = (stack, Just $ score c)
          | c == closes (head stack) = (tail stack,s)
          | otherwise = (stack, Just $ score c)

partA :: Input -> OutputA
partA = sum . mapMaybe (snd . validate scoreA)

------------ PART B ------------
scoreB :: Char -> Int
scoreB ')' = 1
scoreB ']' = 2
scoreB '}' = 3
scoreB '>' = 4

scoreB2 :: String -> Int
scoreB2 = foldl f 0
    where
        f s c = 5 * s + scoreB c

median :: [Int] -> Int
median xs = sorted !! (length xs `div` 2)
    where
        sorted = sort xs

partB :: Input -> OutputB
partB = median . map (scoreB2 . map closes) . getRemainders
    where
        getRemainders = map fst . filter (isNothing . snd) . map (validate scoreA)
