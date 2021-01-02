module Day24 where

import qualified Text.Parsec as P
import qualified Data.Set as S
import Text.Parsec (Parsec)
import Data.Set (Set)
import Linear.V3
import Data.Functor
import Control.Applicative
import Parse
import Common

data Dir = NE | E | SE | SW | W | NW
    deriving (Show, Eq, Ord, Enum)
type Pos = V3 Int
type BTiles = Set Pos

toPos :: [Dir] -> Pos
toPos = foldl step (V3 0 0 0)

asPos :: Dir -> Pos
asPos NE = V3 1 0 (-1)
asPos E  = V3 1 (-1) 0
asPos SE = V3 0 (-1) 1
asPos SW = V3 (-1) 0 1
asPos W  = V3 (-1) 1 0
asPos NW = V3 0 1 (-1)

step :: V3 Int -> Dir -> V3 Int
step p = (p +) . asPos 

day24a :: String -> Int
day24a = solveA . dayInput

solveA :: [Pos] -> Int
solveA = S.size . foldl flipTile S.empty 

flipTile :: BTiles -> Pos -> BTiles
flipTile s p
    | p `S.member` s = S.delete p s
    | otherwise      = S.insert p s

day24b :: String -> Int
day24b = solveB . dayInput

solveB :: [Pos] -> Int
solveB = S.size . (!! 100) . iterate play . foldl flipTile S.empty

play :: BTiles -> BTiles
play s = S.filter (whiteToBlack s) adjs 
    <> (s S.\\ S.filter (blackToWhite s) s)
    where adjs = adjacents s

blackToWhite :: BTiles -> Pos -> Bool
blackToWhite s p = n == 0 || n > 2
    where n = count True $ map (`S.member` s) $ neighbours p

whiteToBlack :: BTiles -> Pos -> Bool
whiteToBlack s p = n == 2
    where n = count True $ map (`S.member` s) $ neighbours p

adjacents :: BTiles -> BTiles
adjacents = S.unions . S.map (S.fromList . neighbours)

neighbours :: Pos -> [Pos]
neighbours p = map ((p+) . asPos) [NE ..]



dayInput :: String -> [Pos]
dayInput = unsafeParse (parseLines1 pos)

pos :: Parsec String () Pos
pos = toPos <$> P.many1 dir

dir :: Parsec String () Dir
dir = P.try (P.string "ne" $> NE)
    <|> P.try (P.string "e" $> E)
    <|> P.try (P.string "se" $> SE)
    <|> P.try (P.string "sw" $> SW)
    <|> P.try (P.string "w" $> W)
    <|> P.try (P.string "nw" $> NW)
