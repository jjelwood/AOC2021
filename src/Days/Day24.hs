module Days.Day24 (runDay) where

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
import Debug.Trace (traceShowId, traceShow)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where
    instruction = choice
      [ Inp <$> (string "inp " *> letter)
      , Add <$> (string "add " *> letter <* char ' ') <*> val
      , Mul <$> (string "mul " *> letter <* char ' ') <*> val
      , Div <$> (string "div " *> letter <* char ' ') <*> val
      , Mod <$> (string "mod " *> letter <* char ' ') <*> val
      , Eql <$> (string "eql " *> letter <* char ' ') <*> val
      ]
    val = choice [ Var <$> letter, Num <$> signedInt ]
    signedInt = do
        sign <- option 1 (char '-' >> return (-1))
        num <- decimal
        return $ sign * num

------------ TYPES ------------
type Input = [Instruction]
data Instruction = Inp Char
                 | Add Char Val
                 | Mul Char Val
                 | Div Char Val
                 | Mod Char Val
                 | Eql Char Val
    deriving (Show, Eq)

data Val = Var Char | Num Int
    deriving (Show, Eq)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- get :: Vector Int -> Char -> Int
-- get memory c = memory Vec.! (fromEnum c - fromEnum 'a')

-- set :: Vector Int -> Char -> Int -> Vector Int
-- set memory c val = memory Vec.// [(fromEnum c - fromEnum 'a', val)]

-- getVal :: Vector Int -> Val -> Int
-- getVal memory (Var c) = get memory c
-- getVal _ (Num n) = n

-- calculateInstructions :: [Int] -> [Instruction] -> Vector Int -> Bool
-- calculateInstructions inputs instructions memory =
--     isValidProg $ snd $ foldl executeInstruction (inputs, memory) instructions

-- executeInstruction :: ([Int], Vector Int) -> Instruction -> ([Int], Vector Int)
-- executeInstruction (inputs, memory) (Inp c) = (tail inputs, set memory c (head inputs))
-- executeInstruction (inputs, memory) (Add c val) = (inputs, set memory c (get memory c + getVal memory val))
-- executeInstruction (inputs, memory) (Mul c val) = (inputs, set memory c (get memory c * getVal memory val))
-- executeInstruction (inputs, memory) (Div c val) = (inputs, set memory c (get memory c `quot` getVal memory val))
-- executeInstruction (inputs, memory) (Mod c val) = (inputs, set memory c (get memory c `rem` getVal memory val))
-- executeInstruction (inputs, memory) (Eql c val) = (inputs, set memory c (if get memory c == getVal memory val then 1 else 0))

-- isValidProg :: Vector Int -> Bool
-- isValidProg memory = get memory 'z' == 0

-- numbersLenN :: Int -> [[Int]]
-- numbersLenN 0 = [[]]
-- numbersLenN n = [x : xs | x <- [9,8..1], xs <- numbersLenN (n - 1)]

concatInts :: [Int] -> Int
concatInts = foldl (\acc x -> acc * 10 + x) 0

-- partA :: Input -> OutputA
-- partA inp = 
--     concatInts $
--     head $ filter (\ds -> calculateInstructions ds inp (Vec.replicate 26 0)) $
--     numbersLenN 14

getPush :: [Instruction] -> Int
getPush is = case is !! 15 of Add c (Num n) -> n; _ -> error "Unexpected input"

getPop :: [Instruction] -> Int
getPop is = case is !! 5 of Add c (Num n) -> n; _ -> error "Unexpected input"

getDigits :: (Int -> (Int, Int)) -> [(Int, Int)] -> [[Instruction]] -> [Int]
getDigits solveF pushPops iss = Vec.toList $ foldl calcNs (Vec.replicate 14 0) pushPops
  where
    calcNs ns (push, pop) = ns Vec.// [(push, nPush), (pop, nPop)]
      where
        (nPop, nPush) = solveF dif
        dif = getPush (iss !! push) + getPop (iss !! pop)

maxNsolveF :: Int -> (Int, Int)
maxNsolveF dif = if dif <= 0 then (9 + dif, 9) else (9, 9 - dif)

minNsolveF :: Int -> (Int, Int)
minNsolveF dif = if dif <= 0 then (1, 1 - dif) else (1 + dif, 1)

makePushPopStack :: [[Instruction]] -> [(Int, Int)]
makePushPopStack iss = fst $ foldl (\(res, stack) (index, is) -> if isPush is then (res, index:stack) else ((head stack, index):res, tail stack))
  ([], []) $ zip [0..] iss

isPop :: [Instruction] -> Bool
isPop is = case is !! 4 of Div _ (Num n) -> n == 26; _ -> error "Unexpected input"

isPush :: [Instruction] -> Bool
isPush is = case is !! 4 of Div _ (Num n) -> n == 1; _ -> error "Unexpected input"

partA :: Input -> OutputA
partA inp = concatInts $ getDigits maxNsolveF (makePushPopStack chunks) chunks
  where
    chunks = U.chunksOf 18 inp

------------ PART B ------------
partB :: Input -> OutputB
partB inp = concatInts $ getDigits minNsolveF (makePushPopStack chunks) chunks
  where
    chunks = U.chunksOf 18 inp

