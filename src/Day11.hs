{-# LANGUAGE RecordWildCards #-}
module Day11 where

import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Text.Parsec as P
import Text.Parsec ((<|>), Parsec)
import Data.Functor (($>))
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe
import Control.Monad.State
import Parse
import Data.List.Split (chunksOf)
import Common ( Point, pAdd, pMul )

day11a :: String -> Int
day11a = solveA . dayInput

solveA :: Env -> Int
solveA e@E{..} = V.length $ V.filter (==Occ) eSeats
    where (_,E{..}) = runState wait e{eCheck = toCheckA, eMinOcc=4}

day11b :: String -> Int
day11b = solveB . dayInput

solveB :: Env -> Int
solveB e@E{..} = V.length $ V.filter (==Occ) eSeats
    where (_,E{..}) = runState wait e{eCheck = toCheckB, eMinOcc=5}

data Seat = Occ | Empty | Floor
    deriving (Show, Ord, Eq)
data Env = E
    { eHeight :: Int
    , eWidth  :: Int
    , eSeats  :: Vector Seat
    , eCheck  :: Env -> Int -> [Int]
    , eMinOcc :: Int
    }

type Seats = Map Point Bool
type NMap = Map Point (Set Point)

type WaitingHall = State Env ()

wait :: WaitingHall
wait = do
    ss <- gets eSeats
    step
    ss' <- gets eSeats
    unless (ss == ss') wait

step :: WaitingHall
step = do
    cs <- gets changes
    modify (apply cs)

apply :: Vector Bool -> Env -> Env
apply cs e@E{..} = e{eSeats = V.map (\(b,s) -> if b then switch s else s) $ V.zip cs eSeats }

switch :: Seat -> Seat
switch Occ   = Empty
switch Empty = Occ
switch Floor = Floor

changes :: Env -> Vector Bool
changes e@E{..} = V.imap (willChange e) eSeats 

toCheckA :: Env -> Int -> [Int]
toCheckA E{..} n = map (idxFromPoint eWidth) $ filter (inBounds eWidth eHeight) $ map (pAdd (pointFromIdx eWidth n)) adjs

adjs :: [Point]
adjs = [(x,y) | x <- [-1,0,1],
                y <- [-1,0,1],
                not (x == 0 && y == 0)]

toCheckB :: Env -> Int -> [Int]
toCheckB e@E{..} n = mapMaybe (checkDir e (pointFromIdx eWidth n)) adjs

checkDir :: Env -> Point -> Point -> Maybe Int
checkDir e p d = checkDir' e p d 1

checkDir' :: Env -> Point -> Point -> Int -> Maybe Int
checkDir' e@E{..} p d n
    | not (inBounds eWidth eHeight p') = Nothing
    | s == Occ   = Just x
    | s == Empty = Nothing
    | otherwise  = checkDir' e p d (n+1)
    where p' = pAdd p $ pMul d (n,n)
          x  = idxFromPoint eWidth p'
          s  = eSeats V.! x

idxFromPoint :: Int -> Point -> Int
idxFromPoint w (x,y) = y * w + x

pointFromIdx :: Int -> Int -> Point
pointFromIdx w n = (n `mod` w, n `quot` w)

inBounds :: Int -> Int -> Point -> Bool
inBounds w h (x,y) = x >= 0 && x < w && y >= 0 && y < h

willChange :: Env -> Int -> Seat -> Bool
willChange _       _   Floor = False
willChange e@E{..} idx s
    | s == Empty = Occ `notElem` adjs'
    | s == Occ   = length (filter (==Occ) adjs') >= eMinOcc
    where adjs' = map (eSeats V.!) $ eCheck e idx

dayInput :: String -> Env
dayInput input = case parse seats (concat ls) of
                    Right im -> E{eHeight=h,eWidth=w,eSeats=im,eMinOcc=0,eCheck= \_ _ -> []}
                    Left e -> error $ show e
    where h = length ls
          w = length l
          ls@(l:_) = lines input


seats :: Parsec String () (Vector Seat) 
seats = V.fromList <$> P.many1 seat

seat :: Parsec String () Seat
seat = P.char 'L' $> Empty <|>
       P.char '.' $> Floor

showSeats :: Vector Seat -> String
showSeats = unlines . chunksOf 10 . V.toList . V.map showSeat

showSeat :: Seat -> Char
showSeat Floor = '.'
showSeat Occ   = '#'
showSeat Empty = 'L'