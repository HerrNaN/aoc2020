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
import Linear hiding (transpose, trace)
import Control.Lens
import Prelude hiding (Right, Left)
import Debug.Trace (traceShow, trace)

type Point = V2 Int
type Border = String
type Contents = [String]
data Transformation = None | Rot90 | Rot180 | Rot270 | VFlip | HFlip | Rot90HFlip | Rot90VFlip
    deriving (Eq, Ord, Enum, Show)
type Puzzle = Map Point Piece
type Image  = [String]
data Piece = P
    { pId       :: PID
    , pEdges    :: Set Edge
    , pContents :: Contents
    } deriving (Eq, Ord, Show)

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
    Up    -> V2 0 (-1)
    Down  -> V2 0 1
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
    where img = trace' $ toImage $ assemble ps
          pImgs = map (`doTransform` img) [None ..]
          nm  = maximum $ map countMonsters pImgs
          ws  = count '#' $ unlines img
          mws = count '#' $ unlines monster

countMonsters :: Image -> Int
countMonsters img = count True $ map (isAMonsterAt ws) $ S.toList ws
    where ws = M.keysSet $ M.filter (== '#') $ asciiGrid0' $ unlines img

isAMonsterAt :: Set (V2 Int) -> V2 Int -> Bool
isAMonsterAt ws w = all ((`S.member` ws) . (+w)) monsterCoords

assemble :: [Piece] -> Puzzle
assemble (p:ps) = snd $ iterate addPiece (em, M.singleton (V2 0 0) (trace' p)) !! (length ps' `quot` 8)
    where em  = edgeMap ps'
          ps' = filter ((/=pId p) . pId) ps

addPiece :: (Map Edge [Piece], Puzzle) -> (Map Edge [Piece], Puzzle)
addPiece (em, pzl) = (em', trace (show $ M.map pId pzl') pzl')
    where (pos, e) = chooseEdge em pzl         -- Choose an edge in the puzzle to find a piece for
          p        = pickPiece (pos, e) em pzl -- Pick out the piece for the chosen edge
          em'      = removePieceFrom p em      -- Remove piece from edgemap 
          pzl'     = addPieceAt pos e p pzl

pickPiece :: (Point, Edge) -> Map Edge [Piece] -> Puzzle -> Piece
pickPiece (pos, (s,b)) em pzl = p'
    where ps   = em M.! (opp s, b)
          pid' = pId (pzl M.! pos)
          ps'  = filter ((/= pid') . pId) ps
          p'   = head ps'

addPieceAt :: Point -> Edge -> Piece -> Puzzle -> Puzzle
addPieceAt pos (s,_) p = M.insert pos' $ traceLabel "addPieceAt: " p
    where pos' = pos + fromSide s
    

removePieceFrom :: Piece -> Map Edge [Piece] -> Map Edge [Piece]
removePieceFrom p = M.map (filter ((/=pId p) . pId))

chooseEdge :: Map Edge [Piece] -> Puzzle -> (Point, Edge)
chooseEdge em pzl = traceLabel "chooseEdge: " $ (pos, head edges)
    where es = danglingEdges em pzl
          (pos, edges) = head es

danglingEdges :: Map Edge [Piece] -> Puzzle -> [(Point, [Edge])]
danglingEdges em pzl = traceLabel "danglingEdges: " . M.toList . M.filter (not . null) . M.map (danglingEdges' placed em) $ pzl
    where placed = S.fromList $ M.elems $ M.map pId pzl

danglingEdges' ::  Set PID  -> Map Edge [Piece] -> Piece -> [Edge]
danglingEdges' placed em p = traceLabel "danglingEdges': " . S.toList $ S.filter (isJust . fitsWithARemainingPiece placed i em) $ pEdges p
    where i = pId $ traceLabel "thisPiece: " p

fitsWithARemainingPiece :: Set PID -> PID -> Map Edge [Piece] -> Edge -> Maybe Edge
fitsWithARemainingPiece placed thisPID em (s,b) = do
    let e = (opp s, b)
    es <- em M.!? e
    traceLabel "fitsWithAremainingPiece: " $
        if any ((`S.notMember` placed') . pId) es then Just (s,b) else Nothing
    where placed' = S.insert (traceLabel "thisPID: " thisPID) placed

rotCw90 :: [[a]] -> [[a]]
rotCw90 = transpose . reverse

assemblePieces :: Set PID -> IntMap [PID] ->  [PID]
assemblePieces pSet bm = []
    where (p:ps) = S.elems pSet

groupByX :: [(Point, Contents)] -> [[(Point, Contents)]]
groupByX = groupBy (\v v' -> (fst v ^._x) == (fst v' ^._x))

toImage :: Puzzle -> Image
toImage = foldl (zipWith (++)) (repeat "") . map (concatMap snd) . groupByX . sort . M.toList . M.map (stripBorders . pContents)
-- toImage pzl = concatMap linkX $ groupByX $ M.toList $ M.map pContents pzl

linkX :: [(Point, Contents)] -> [String]
linkX ps = foldl (zipWith (++)) (repeat "") css
    where css = map snd ps 

corners :: [Piece] -> [PID]
corners ps = S.toList $ S.fromList $ map pId $ filter (isCorner em) ps
    where em = edgeMap ps

isCorner :: Map Edge [Piece] -> Piece -> Bool
isCorner em pp = 2 == numberOfUsableEdges em pp

numberOfUsableEdges :: Map Edge [Piece] -> Piece -> Int
numberOfUsableEdges em P{..} = length $ S.filter (\(s,b) -> length (em M.! (opp s, b)) > 1) pEdges

edgeMap :: [Piece] -> Map Edge [Piece]
edgeMap = foldl addEdges M.empty

addEdges :: Map Edge [Piece] -> Piece -> Map Edge [Piece]
addEdges em p@P{..} = M.unionWith (++) em $ M.fromList $ zip (S.elems pEdges) $ repeat [p]

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
mkPiece pId cs t = P pId es cs'
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
