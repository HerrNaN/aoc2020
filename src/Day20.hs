{-# LANGUAGE OverloadedStrings #-}
module Day20 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Set (Set)
import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import Parse
import Common
import Data.List.Split
import Data.List


type Border = String
type Piece  = (Int, [Border])
type IPiece = (Int, [Int])

day20a :: String -> Int
day20a = solveA . dayInput

solveA :: [IPiece] -> Int
solveA = product . corners

day20b :: String -> Int
day20b = solveB . dayInput

solveB :: [IPiece] -> Int
solveB = error "not implemented"

borderMap :: [IPiece] -> IntMap [Int]
borderMap = foldl addBorders IM.empty

addBorders :: IntMap [Int] -> IPiece -> IntMap [Int]
addBorders m (p, bs) = IM.unionWith (++) m $ IM.fromList $ zip bs $ repeat [p]

{-|
  Why does this work?

-}
corners :: [IPiece] -> [Int]
corners = map head
        . filter ((== 4) . length)
        . group
        . sort
        . concat
        . IM.elems
        . IM.filter ((== 1) . length)
        . borderMap

borderToInt :: Border -> Int
borderToInt = fromInteger . fromBin . map (\c -> if c == '#' then '1' else '0')

dayInput :: String -> [IPiece]
dayInput i = ps'
    where ps = splitOn "\n\n" (T.unpack (T.strip $ T.pack i))
          ps' = map (parsePiece . lines) ps

parsePiece :: [String] -> (Int, [Int])
parsePiece []     = error "trying to parse piece from empty list"
parsePiece (l:ls) = (pid, bs)
    where pid = unsafeParse pieceId l
          bs  = borders ls

borders :: [String] -> [Int]
borders ss = map borderToInt [top, reverse top, left, reverse left, right, reverse right, bottom, reverse bottom]
    where top    = head ss
          left   = head $ transpose ss
          right  = last $ transpose ss
          bottom = last ss

pieceId :: Parsec String () Int
pieceId = P.between (P.string "Tile ") (P.string ":") parseInt
