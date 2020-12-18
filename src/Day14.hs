{-# LANGUAGE RecordWildCards #-}
module Day14 where

import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec ((<|>), Parsec)
import qualified Text.Parsec as P
import Numeric (showIntAtBase)
import Data.Char (digitToInt, intToDigit)
import Parse

type Addr = Integer
type Mask = String
type Memory = Map Integer String
type FAddr = String
data Instruction = SetMask Mask | WriteTo Addr Integer deriving (Show)

data Prog = P
    { _mask :: Mask
    , _mem  :: Memory
    } deriving (Show)

day14a :: String -> Integer
day14a = solveA . dayInput

solveA :: [Instruction] -> Integer
solveA is = M.foldl (+) 0 $ M.map fromBin mem'
    where mem' = _mem $ foldl doInstr P{_mask="",_mem=M.empty} is

doInstr :: Prog -> Instruction -> Prog
doInstr p@P{..} (SetMask m')       = p{_mask=m'}
doInstr p@P{..} (WriteTo addr val) = p{_mem=mem'}
    where mem' = M.insert addr (applyMask partABit _mask (toBin val)) _mem

fromBin :: String -> Integer
fromBin s = sum $ zipWith (*) (map toInteger $ reverse (map digitToInt s)) $ map (2^) [0..]

toBin :: Integer -> String
toBin i = showIntAtBase 2 intToDigit i ""

applyMask :: (Char -> Char -> Char) -> Mask -> String -> String
applyMask f m s = zipWith f m s'
    where toPad = 36 - length s
          s'    = replicate toPad '0' ++ s

partABit :: Char -> Char -> Char
partABit 'X' c' = c'
partABit c   _  = c

day14b :: String -> Integer
day14b = solveB . dayInput

solveB :: [Instruction] -> Integer
solveB is = M.foldl (+) 0 $ M.map fromBin mem'
    where mem' = _mem $ foldl doInstrB P{_mask="",_mem=M.empty} is

addrs :: FAddr -> [String]
addrs ['X']    = ["1","0"]
addrs [b]      = [[b]]
addrs ('X':bs) = map ('1':) (addrs bs) ++ map ('0':) (addrs bs)
addrs (b:bs)   = map (b:) $ addrs bs

partBBit :: Char -> Char -> Char
partBBit '0' c = c
partBBit m   _ = m

doInstrB :: Prog -> Instruction -> Prog
doInstrB p@P{..} (SetMask m')  = p{_mask=m'}
doInstrB p@P{..} (WriteTo a v) = p{_mem=mem'}
    where mem' = insertMany (map fromBin as) (showIntAtBase 2 intToDigit v "") _mem
          fa = applyMask partBBit _mask (toBin a)
          as = addrs fa


insertMany :: [Addr] -> String -> Memory -> Memory
insertMany []     _ m = m
insertMany (a:as) v m = M.insert a v $ insertMany as v m

dayInput :: String -> [Instruction]
dayInput = unsafeParse instructions

instructions :: Parsec String () [Instruction]
instructions = P.sepEndBy1 instruction P.newline

instruction :: Parsec String () Instruction
instruction = P.try mask <|> write

mask :: Parsec String () Instruction
mask = do
    P.string "mask = "
    SetMask <$> P.many1 (P.oneOf "X10")

write :: Parsec String () Instruction
write = do
    P.string "mem["
    a <- parseInt
    P.string "] = "
    WriteTo (toInteger a) . toInteger <$> parseInt