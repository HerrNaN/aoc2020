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
type Puzzle = Map Point Piece
type Image  = [String]
data Piece = PP
    { pId       :: PID
    , pEdges    :: Set Edge
    , pContents :: Contents
    } deriving (Eq, Show)

type Edge = (Side, Border)
data Side = Up | Down | Right | Left
    deriving (Show, Ord, Eq, Enum)
type PID = Int

opp :: Side -> Side
opp = \case
    Up    -> Down
    Down  -> Up
    Left  -> Right
    Right -> Left

fromSide :: Side -> V2 Int
fromSide = \case 
    Up    -> V2 0 1
    Down  -> V2 0 (-1)
    Right -> V2 1 0
    Left  -> V2 (-1) 0

monster :: [String]
monster = [ "                  # "
          , "#    ##    ##    ###"
          , " #  #  #  #  #  #   " ]

-- With a center point on the end of the tail
monsterCoords :: [V2 Int]
monsterCoords = [V2 18 (-1) 
                ,V2 0 0, V2 5 0, V2 6 0, V2 11 0, V2 12 0, V2 17 0, V2 18 0, V2 19 0
                ,V2 1 1, V2 4 1, V2 7 1, V2 10 1, V2 13 1, V2 16 1]

count :: Eq a => a -> [a] -> Int
count a = countTrue (==a)

day20a :: String -> Int
day20a = solveA . dayInput

solveA :: [Piece] -> Int
solveA = product . corners

day20b :: String -> Int
day20b = solveB . dayInput

solveB :: [Piece] -> Int
solveB ps = ws - (nm * mws)
    where img = toImage $ assemble ps
          nm  = countMonsters img
          ws  = count '#' $ unlines img
          mws = count '#' $ unlines monster

countMonsters :: Image -> Int
countMonsters img = count True $ S.toList $ S.map (isAMonsterAt ws) ws
    where ws = M.keysSet $ M.filter (== '#') $ asciiGrid0' $ unlines img

isAMonsterAt :: Set (V2 Int) -> V2 Int -> Bool
isAMonsterAt ws w = all ((`S.member` ws) . (+w)) monsterCoords

assemble :: [Piece] -> Puzzle
assemble ps = snd $ iterate addPiece (em, M.singleton (V2 0 0) (head ps)) !! length ps
    where em = edgeMap ps

addPiece :: (Map Edge [Piece], Puzzle) -> (Map Edge [Piece], Puzzle)
addPiece (em, pzl) = (em', pzl')
    where em'  = error "not implemented"
          pzl' = error "not implemented"
          e    = chooseEdge em pzl
          p    = getPiece e em pzl

getPiece :: t0 -> Map Edge [Piece] -> Puzzle -> t
getPiece = error "not implemented"

chooseEdge :: Map Edge [Piece] -> Puzzle -> t
chooseEdge = error "not implemented"

rotCw90 :: [[a]] -> [[a]]
rotCw90 = transpose . reverse

assemblePieces :: Set PID -> IntMap [PID] ->  [PID]
assemblePieces pSet bm = []
    where (p:ps) = S.elems pSet

groupByX :: [(Point, Contents)] -> [[(Point, Contents)]]
groupByX = groupBy (\v v' -> (fst v ^._x) == (fst v' ^._x))

toImage :: Puzzle -> Image
toImage pzl = concatMap linkX $ groupByX $ M.toList $ M.map pContents pzl

linkX :: [(Point, Contents)] -> [String]
linkX ps = foldl (zipWith (++)) (repeat "") css
    where css = map snd ps 

corners :: [Piece] -> [PID]
corners ps = S.toList $ S.fromList $ map pId $ filter (isCorner em) ps
    where em = edgeMap ps

isCorner :: Map Edge [Piece] -> Piece -> Bool
isCorner em pp = 2 == numberOfUsableEdges em pp

numberOfUsableEdges :: Map Edge [Piece] -> Piece -> Int
numberOfUsableEdges em PP{..} = length $ S.filter (\(s,b) -> length (em M.! (opp s, b)) > 1) pEdges

edgeMap :: [Piece] -> Map Edge [Piece]
edgeMap = foldl addEdges M.empty

addEdges :: Map Edge [Piece] -> Piece -> Map Edge [Piece]
addEdges em p@PP{..} = M.unionWith (++) em $ M.fromList $ zip (S.elems pEdges) $ repeat [p]

-- This conversion shouldn't be neccesary but helps alot with debugging 
borderToInt :: Border -> Int
borderToInt = fromInteger . fromBin . map (\c -> if c == '#' then '1' else '0')

dayInput :: String -> [Piece]
dayInput i = ps'
    where ps = splitOn "\n\n" (T.unpack (T.strip $ T.pack i))
          ps' = concatMap (parsePieces . lines) ps

parsePieces :: [String] -> [Piece]
parsePieces (l:ls) = fromTemplate pId ls
    where pId = unsafeParse pid l
          
stripBorders :: [String] -> [String]
stripBorders = map (init . tail) . init . tail

fromTemplate :: PID -> Contents -> [Piece]
fromTemplate pId cs = map (mkPiece pId cs) [None ..]

mkPiece :: PID -> Contents -> Transformation -> Piece
mkPiece pId cs t = PP pId es cs'
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
