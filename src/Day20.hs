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
import Linear hiding (transpose)
import Control.Lens
import Prelude hiding (Right, Left)

type Point = V2 Int
type Border = String
type Contents = [String]
data Transformation = None | Rot90 | Rot180 | Rot270 | VFlip | HFlip | Rot90HFlip | Rot90VFlip
    deriving (Eq, Ord, Enum, Show)
type Puzzle = Map Point Contents
type Image  = [String]
data Piece = P
    { pId :: Int
    , pBorders :: Map Transformation (Set Border)
    , pContents :: Map Transformation Contents
    } deriving (Eq, Show)

data PPiece = PP
    { ppId :: PID
    , ppEdges :: Set Edge
    , ppContents :: Contents
    } deriving (Eq, Show)
type Edge = (Side, Border)

data Side = Up | Down | Right | Left
    deriving (Show, Ord, Eq, Enum)
type PID = Int
type ConnectionMap = Map Edge (Set Piece)

opp :: Side -> Side
opp Up    = Down
opp Down  = Up
opp Left  = Right
opp Right = Left

monster :: [String]
monster = [ "                  # "
          , "#    ##    ##    ###"
          , " #  #  #  #  #  #   " ]

-- With a center point on the end of the tail
monsterCoords :: [V2 Integer]
monsterCoords = [V2 18 1 
                ,V2 0 0, V2 5 0, V2 6 0, V2 11 0, V2 12 0, V2 17 0, V2 18 0, V2 19 0
                ,V2 1 1, V2 4 1, V2 7 1, V2 10 1, V2 13 1, V2 16 1]

count :: Eq a => a -> [a] -> Int
count a = countTrue (==a)

day20a :: String -> Int
day20a = solveA . dayInput

solveA :: [PPiece] -> Int
solveA = product . corners'

day20b :: String -> Int
day20b = solveB . dayInput

solveB :: [PPiece] -> Int
solveB ps = ws - (nm * mws)
    where img = toImage $ assemble ps
          nm  = countMonsters img
          ws  = count '#' $ unlines img
          mws = count '#' $ unlines monster

countMonsters :: Image -> Int
countMonsters = error "not implemented"

assemble :: [PPiece] -> Puzzle
assemble = error "not implemented"

borderMap :: [Piece] -> Map Border [PID]
borderMap = foldl addBorders M.empty

addBorders :: Map Border [PID] -> Piece -> Map Border [PID]
addBorders m P{..} = M.unionWith (++) m $ M.fromList $ zip (S.elems $ S.unions $ M.elems pBorders) (repeat [pId])

rotCw90 :: [[a]] -> [[a]]
rotCw90 = transpose . reverse

assemblePieces :: Set PID -> IntMap [PID] ->  [PID]
assemblePieces pSet bm = []
    where (p:ps) = S.elems pSet

validBordersMap :: [Piece] -> Map Border [PID]
validBordersMap ps   = M.withoutKeys (borderMap ps) edges
    where edges      = M.keysSet $ M.filter ((== 1) . length) $ borderMap ps
          allBorders = M.keysSet $ borderMap ps

groupByX :: [(Point, Contents)] -> [[(Point, Contents)]]
groupByX = groupBy (\v v' -> (fst v ^._x) == (fst v' ^._x))

toImage :: Puzzle -> Image
toImage pzl = concatMap linkX $ groupByX $ M.toList pzl

linkX :: [(Point, Contents)] -> [String]
linkX ps = foldl (zipWith (++)) (repeat "") css
    where css = map snd ps 

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
-- corners :: [PPiece] -> [PID]
-- corners = map head
--         . filter ((== 4) . length)
--         . group
--         . sort
--         . concat
--         . M.elems
--         . M.filter ((== 1) . length)
--         . borderMap

corners' :: [PPiece] -> [PID]
corners' pps = S.toList $ S.fromList $ map ppId $ filter (isCorner em) pps
    where em = edgeMap pps

isCorner :: Map Edge [PID] -> PPiece -> Bool
isCorner em pp = 2 == numberOfUsableEdges em pp

numberOfUsableEdges :: Map Edge [PID] -> PPiece -> Int
numberOfUsableEdges em PP{..} = length $ S.filter (\(s,b) -> length (em M.! (opp s, b)) > 1) ppEdges

edgeMap :: [PPiece] -> Map Edge [PID]
edgeMap = foldl addEdges M.empty

addEdges :: Map Edge [PID] -> PPiece -> Map Edge [PID]
addEdges em PP{..} = M.unionWith (++) em $ M.fromList $ zip (S.elems ppEdges) $ repeat [ppId]

-- This conversion shouldn't be neccesary but helps alot with debugging 
borderToInt :: Border -> Int
borderToInt = fromInteger . fromBin . map (\c -> if c == '#' then '1' else '0')

dayInput :: String -> [PPiece]
dayInput i = ps'
    where ps = splitOn "\n\n" (T.unpack (T.strip $ T.pack i))
          ps' = concatMap (parsePPieces . lines) ps

parsePiece :: [String] -> Piece
parsePiece (l:ls) = mkPiece pId ls
    where pId = unsafeParse pid l

parsePPieces :: [String] -> [PPiece]
parsePPieces (l:ls) = fromTemplate pId ls
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

fromTemplate :: PID -> Contents -> [PPiece]
fromTemplate pId cs = map (mkPPiece pId cs) [None ..]

mkPPiece :: PID -> Contents -> Transformation -> PPiece
mkPPiece pId cs t = PP pId es cs'
    where cs' = doTransform t cs
          es  = edges cs'

edges :: Contents -> Set Edge
edges cs = S.fromList $ map (\s -> (s, borderFrom cs s)) [Up ..]

borderFrom :: Contents -> Side ->  Border
borderFrom cs = \case
    Up -> head cs
    Down -> last cs
    Left -> head $ transpose cs
    Right -> last $ transpose cs

doTransform :: Transformation -> Contents -> Contents
doTransform None   = id
doTransform Rot90  = (!! 1) . iterate rotCw90
doTransform Rot180 = (!! 2) . iterate rotCw90
doTransform Rot270 = (!! 3) . iterate rotCw90
doTransform HFlip  = reverse
doTransform VFlip  = map reverse
doTransform Rot90HFlip = doTransform HFlip . doTransform Rot90
doTransform Rot90VFlip = doTransform VFlip . doTransform Rot90

possibleBorders :: [Border] -> [Border]
possibleBorders ss = map reverse bs ++ bs
    where bs = borders ss

borders :: [String] -> [Border]
borders ss = map head $ take 4 $ drop 1 $ iterate rotCw90 ss

pid :: Parsec String () Int
pid = P.between (P.string "Tile ") (P.string ":") parseInt
