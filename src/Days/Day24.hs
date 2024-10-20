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

-- The entire program is this loop 14 times:
-- ```
-- inp w
-- mul x 0
-- add x z
-- mod x 26
-- div z (1|1|1|1|26|26|1|26|1|26|1|26|26|26)
-- add x (11|11|14|11|-8|-5|11|-13|12|-1|14|-5|-4|-8)
-- eql x w
-- eql x 0
-- mul y 0
-- add y 25
-- mul y x
-- add y 1
-- mul z y
-- mul y 0
-- add y w
-- add y (1|11|1|11|2|9|7|11|6|15|7|1|8|6)
-- mul y x
-- add z y
-- ```
-- In one iteration
-- $$
-- \begin{align}
-- w & =c_{1} \\
-- x & =0 \\
-- x & = x+z=z \\
-- x & = z \mod 26
-- \end{align}
-- $$
-- $$
-- \begin{align}
-- z & = z \mid z / 26 \\
-- x & = x + a_{1} \\
-- \end{align}
-- $$
-- Then the combination of `eql x w` and `eql x 0` leaves $x=1$ if $x\neq w$ or $x=0$ if $x=w$
-- $$
-- \begin{align}
-- y & =0 \\
-- y & =25 \\
-- y & =25x \\
-- y & =25x+1 \implies y \text{ is either 1 or 26} \\
-- z & = yz \\
-- y & = 0 \\
-- y & = w \\
-- y & = w + a_{2} \\
-- y & = x(w+a_{2}) \\
-- z & = z + x(w+a_{2})
-- \end{align}
-- $$
-- $x$ and $y$ are reset to 0 every loop. $w$ is set to the input digit every loop. $z$ is the only thing that persists.

-- $x$ is set to $z \mod 26 + a_{1}$.

-- $z$ is divided by 1, or 26

-- $x$ is set to 0 if $w$ is equal to $z \mod 26 + a_{1}$ otherwise it's 1

-- If $w$ is equal to $z \mod 26 + a_{1}$ then $y$ is 1, otherwise it's 26.

-- $z$ is incremented by $x(w+c_{i})$

-- ## Simplifications

-- When $a_{1}=1$ $z$ is divided by 1 so is unchanged, so is unchanged, and $a_{1}$ is always larger than 10 (11,11,14,11), so $z \mod 26 + a_{1}$ is never equal to $w$, meaning that it simplifies to
-- $$
-- \begin{align}
-- w & =c_{i} \\
-- z  & = 26 z \\
-- z & = z +w+ c_{i}
-- \end{align}
-- $$
-- When $a_{1}=26$ $z$ is divided by 26, and the remainder discarded. To get to 0 at the end we need to remove all the $c_{i}$s added to $z$, and not add any more. $z$ is like a base 26 stack.

-- $z \mod 26$ gets the last added digit $a_{2}$, and so $\underbrace{ w_{push}+a_{2,push } }_{ \text{pushed digit} }+a_{1,pop}=w_{pop}$  
-- 5 pops 4 $\implies w_{4}+11-8=w_{5}$
-- 6 pops 3 $\implies w_{3}+1-5=w_{6}$
-- 8 pops 7 $\implies w_{7}+7-13=w_{8}$
-- 10 pops 9 $\implies w_{9}+6-1=w_{10}$
-- 12 pops 11 $\implies w_{11}+7-5=w_{12}$
-- 13 pops 2 $\implies w_{2}+11-4=w_{13}$
-- 14 pops 1 $\implies w_{1}+1-8=w_{14}$

-- a1 = 11|11|14|11|-8|-5|11|-13|12|-1|14|-5|-4|-8
-- a2 = 1|11|1|11|2|9|7|11|6|15|7|1|8|6

-- $$
-- \begin{align}
-- w_{4}-3 & =w_{5} \\
-- w_{3}-4 & =w_{6} \\
-- w_{7} -6 & =w_{8} \\
-- w_{9}+5 & =w_{10} \\
-- w_{11}+2 & =w_{12} \\
-- w_{2}+7 & =w_{13} \\
-- w_{1}-7 & =w_{14}
-- \end{align}
-- $$
