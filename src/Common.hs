{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Common (
    freq,
    firstRepeat,
    findFirstCons,
    Point,
    pAdd,
    pSub,
    pMul,
    pOp,
    Vector2D,
    mkVec2D,
    idxFromPoint,
    pointFromIdx,
    inBounds
) where

import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Vector (Vector())
import qualified Data.Vector as Vec
import Data.List (group)

freq :: Ord a => [a] -> Map a Int
freq = Map.fromListWith (+) . map (,1)

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = firstRepeat' Set.empty

firstRepeat' :: Ord a => Set a -> [a] -> Maybe a
firstRepeat' _ [] = Nothing
firstRepeat' seen (x:xs)
    | Set.member x seen = Just x
    | otherwise = firstRepeat' (Set.insert x seen) xs

findFirstCons :: Eq a => [a] -> a
findFirstCons = head . head . filter ((>1) . length) . group

type Point = (Int, Int)

pOp :: (Int -> Int -> Int) -> Point -> Point -> (Int, Int)
pOp op (a,b) (c,d) = (op a c, op b d)

pAdd :: Point -> Point -> (Int, Int)
pAdd = pOp (+)

pSub :: Point -> Point -> (Int, Int)
pSub = pOp (-)

pMul :: Point -> Point -> (Int, Int)
pMul = pOp (*)

data Vector2D a = Vec2D
    { _vec :: Vector a
    , _width :: Int
    , _height :: Int
    } deriving (Eq)

mkVec2D :: Int -> Int -> Vector2D Int
mkVec2D w h = Vec2D
    { _vec=Vec.replicate (w*h) 0
    , _width=w
    , _height=h
    }

idxFromPoint :: Vector2D a -> Point -> Int
idxFromPoint Vec2D{..} (x,y) = y * _width + x

pointFromIdx :: Vector2D a -> Int -> Point
pointFromIdx Vec2D{..} n = (n `mod` _width, n `quot` _width)

inBounds :: Vector2D a -> Point -> Bool
inBounds Vec2D{..} (x,y) = x >= 0 && x < _width && y >= 0 && y < _height

