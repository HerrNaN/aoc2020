{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Common (
    freq,
    firstRepeat,
    fixedPoint,
    countTrue,
    steps,
    Vector2D,
    mkVec2D,
) where

import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Vector (Vector())
import qualified Data.Vector as Vec
import Data.List (group)
import Data.Foldable (Foldable(toList))

freq :: Ord a => [a] -> Map a Int
freq = Map.fromListWith (+) . map (,1)

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = firstRepeat' Set.empty

firstRepeat' :: Ord a => Set a -> [a] -> Maybe a
firstRepeat' _ [] = Nothing
firstRepeat' seen (x:xs)
    | Set.member x seen = Just x
    | otherwise = firstRepeat' (Set.insert x seen) xs

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a = if a == b then a else fixedPoint f b
    where b = f a

steps :: (t -> t) -> t -> [t]
steps f s = s : steps f s'
    where s' = f s

countTrue :: (Foldable f) => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

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

