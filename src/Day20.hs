{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Text (dropEnd)
import Data.Maybe


type Border = String
type Contents = [String]
data Transformation = None | Rot90 | Rot180 | Rot270 | VFlip | HFlip
    deriving (Eq, Ord, Enum, Show)

data Piece = P
    { pId :: Int
    , pBorders :: Map Transformation (Set Border)
    , pContents :: Map Transformation Contents
    } deriving (Eq, Show)

type IPiece = (Int, [Int])
type PID = Int

day20a :: String -> Int
day20a = solveA . dayInput

solveA :: [Piece] -> Int
solveA = product . corners

day20b :: String -> Int
day20b = solveB . dayInput

solveB :: [Piece] -> Int
solveB = error "not implemented"

borderMap :: [Piece] -> Map Border [PID]
borderMap = foldl addBorders M.empty

addBorders :: Map Border [PID] -> Piece -> Map Border [PID]
addBorders m P{..} = M.unionWith (++) m $ M.fromList $ zip (S.elems $ S.unions $ M.elems pBorders) (repeat [pId])

rotCw90 :: [[a]] -> [[a]]
rotCw90 = transpose . reverse

assemblePieces :: Set PID -> IntMap [PID] ->  [PID]
assemblePieces pSet bm = []
    where (p:ps) = S.elems pSet

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
corners :: [Piece] -> [PID]
corners = map head
        . filter ((== 4) . length)
        . group
        . sort
        . concat
        . M.elems
        . M.filter ((== 1) . length)
        . borderMap

-- This conversion shouldn't be neccesary but helps alot with debugging 
borderToInt :: Border -> Int
borderToInt = fromInteger . fromBin . map (\c -> if c == '#' then '1' else '0')

dayInput :: String -> [Piece]
dayInput i = ps'
    where ps = splitOn "\n\n" (T.unpack (T.strip $ T.pack i))
          ps' = map (parsePiece . lines) ps

parsePiece :: [String] -> Piece
parsePiece (l:ls) = mkPiece pId ls
    where pId = unsafeParse pid l
          
stripBorders :: [String] -> [String]
stripBorders = map (init . tail) . init . tail

mkPiece :: PID -> Contents -> Piece
mkPiece pId cs = P
    { pId=pId
    , pBorders=bm
    , pContents=cm
    }
    where cm = M.fromList $ zip [None ..] $ map (`doTransform` cs) [None ..]
          bm = M.map (S.fromList . borders) cm

doTransform :: Transformation -> Contents -> Contents
doTransform None   = id
doTransform Rot90  = (!! 1) . iterate rotCw90
doTransform Rot180 = (!! 2) . iterate rotCw90
doTransform Rot270 = (!! 3) . iterate rotCw90
doTransform HFlip  = reverse
doTransform VFlip  = map reverse

possibleBorders :: [Border] -> [Border]
possibleBorders ss = map reverse bs ++ bs
    where bs = borders ss

borders :: [String] -> [Border]
borders ss = map head $ take 4 $ drop 1 $ iterate rotCw90 ss

pid :: Parsec String () Int
pid = P.between (P.string "Tile ") (P.string ":") parseInt
