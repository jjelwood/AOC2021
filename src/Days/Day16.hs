{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
module Days.Day16 (runDay) where

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
import qualified Data.Text as T
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Packet
inputParser = do
    packetify <$> many anyChar

packetP :: Parser Packet
packetP = do
    version <- fromIntegral <$> binaryP 3
    typeId <- fromIntegral <$> binaryP 3
    case typeId of
        4 -> do -- Literal Packet
            ds <- many (char '1' *> count 4 digit)
            fd <- char '0' *> count 4 digit
            return (Literal version typeId (binToDec (concat ds ++ fd)) (6 + 5 * (length ds + 1)))
        _ -> do -- Operator Packet
            lengthType <- binaryP 1
            if lengthType == 0
                then do -- Bit length
                    bitLength <- fromIntegral <$> binaryP 15
                    subpackets <- subpacketsBitLengthP bitLength
                    return (Operator version typeId subpackets (6 + 1 + 15 + sum (map bits subpackets)))
                else do -- Packet length
                    subpacketsLength <- fromIntegral <$> binaryP 11
                    subpackets <- count subpacketsLength packetP
                    return (Operator version typeId subpackets (6 + 1 + 11 + sum (map bits subpackets)))

subpacketsBitLengthP :: Int -> Parser [Packet]
subpacketsBitLengthP 0 = return []
subpacketsBitLengthP n = do
    p <- packetP
    ps <- subpacketsBitLengthP (n - bits p)
    return (p : ps)
                    

binaryP :: Int -> Parser Integer
binaryP n = do
    ds <- count n digit
    return (binToDec ds)

binToDec :: String -> Integer
binToDec = foldl (\acc d -> acc * 2 + read [d]) 0

------------ TYPES ------------
type Input = Packet
data Packet = Literal { version :: Int, typeID :: Int, value :: Integer, bits :: Int } |
              Operator { version :: Int, typeID :: Int, subpackets :: [Packet], bits :: Int }
    deriving (Eq, Show)

type OutputA = Int

type OutputB = Integer

------------ PART A ------------
hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

hexStrToBin :: String -> String
hexStrToBin = concatMap hexToBin

sumVns :: Packet -> Int
sumVns (Literal v _ _ _) = v
sumVns (Operator v _ ps _) = v + sum (map sumVns ps)

packetify :: String -> Packet
packetify inp = case parseOnly packetP $ T.pack (hexStrToBin inp) of
    Left e -> error e
    Right p -> p

partA :: Input -> OutputA
partA = sumVns

------------ PART B ------------
eval :: Packet -> Integer
eval (Literal _ _ v _) = fromIntegral v
eval (Operator _ 0 ps _) = sum (map eval ps)
eval (Operator _ 1 ps _) = product (map eval ps)
eval (Operator _ 2 ps _) = minimum (map eval ps)
eval (Operator _ 3 ps _) = maximum (map eval ps)
eval (Operator _ 5 ps _) = if ((>) <$> (!! 0) <*> (!! 1)) (map eval ps) then 1 else 0
eval (Operator _ 6 ps _) = if ((<) <$> (!! 0) <*> (!! 1)) (map eval ps) then 1 else 0
eval (Operator _ 7 ps _) = if ((==) <$> (!! 0) <*> (!! 1)) (map eval ps) then 1 else 0

partB :: Input -> OutputB
partB = eval
