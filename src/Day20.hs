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
  - Why does this work?

  It works on the assumption that there is only one right fit
  for a piece with any of the other pieces. So for example,
  piece A only fits with piece B and only in one way.
  The borders are a unique link between two pieces.

  - How does it work?

  First we map all the borders to a list of pieces.
  This will allow us to lookup what pieces have a specific
  border. Then we filter out all borders that aren't shared
  between two pieces. Those should be the borders of the actual
  puzzle. Now we get all the pieces that have these borders (the
  edge pieces of the puzzle). We then need to find out which four of
  these are the corner pieces. Naturally the corner pieces will have
  two borders that aren't shared with any other piece so they should
  appear two times (each) in the list of edge pieces (since every
  occurance here represents having one unique border) and since we
  allowed for rotations and flips we will have the flipped version of
  each of the edge borders, so our corners will actially  appear
  four times in total (each). Then we just get the actual ids of
  the pieces.
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
