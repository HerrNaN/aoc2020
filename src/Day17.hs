{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Day17 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Vector as Vec
import Data.Functor
import Control.Applicative
import Parse
import Common ( countTrue )
import Data.Maybe
import Linear.V3 ( V3(..) )
import Linear.V4 ( V4(..) )

type Point3 = V3 Int
type Point4 = V4 Int
-- type Space = Map Point3 Bool
type Space3 = Map Point3 Bool
type Space4 = Map Point4 Bool
-- type Neighbours = Map (V3 Int) (Set (V3 Int))

day17a :: String -> Int
day17a = solveA . dayInput3

solveA :: Space3 -> Int
solveA = countTrue (==True) . (!! 6) . iterate step3

day17b :: String -> Int
day17b = solveB . dayInput4

solveB :: Space4 -> Int
solveB = countTrue id . (!! 6) . iterate step4

step3 :: Space3 -> Space3
step3 gd = M.filter id $  step3' as gd
    where as = M.mapWithKey (\k _ -> activeNeighbours3 gd k) $ toConsider3 gd

step4 :: Space4 -> Space4
step4 gd = M.filter id $  step4' as gd
    where as = M.mapWithKey (\k _ -> activeNeighbours4 gd k) $ toConsider4 gd

step3' :: Map Point3 Int -> Space3 -> Space3
step3' anm gd = M.mapWithKey switch anm
    where switch k an = case gd M.!? k of
                            Just True  -> an == 2 || an == 3
                            Just False -> an == 3
                            Nothing    -> an == 3

step4' :: Map Point4 Int -> Space4 -> Space4
step4' anm gd = M.mapWithKey switch anm
    where switch k an = case gd M.!? k of
                            Just True  -> an == 2 || an == 3
                            Just False -> an == 3
                            Nothing    -> an == 3

toConsider3 :: Space3 -> Space3
toConsider3 g = consider
    where active = M.filter id g
          nbhMaps = map (\v -> M.fromList $ map (\x -> (x+v, False)) adjs3) $ M.keys active
          consider = M.unionsWith (||) (active:nbhMaps)
          
toConsider4 :: Space4 -> Space4
toConsider4 g = consider
    where active = M.filter id g
          nbhMaps = map (\v -> M.fromList $ map (\x -> (x+v, False)) adjs4) $ M.keys active
          consider = M.unionsWith (||) (active:nbhMaps)

activeNeighbours3 :: Space3 -> Point3 -> Int
activeNeighbours3 g v = countTrue (==True) $ map ((\b -> isJust b && fromJust b) . (g M.!?)) as
    where as = map (+v) adjs3

activeNeighbours4 :: Space4 -> Point4 -> Int
activeNeighbours4 g v = countTrue (==True) $ map ((\b -> isJust b && fromJust b) . (g M.!?)) as
    where as = map (+v) adjs4

adjs3 :: [Point3]
adjs3 = [V3 x y z | x <- [-1,0,1],
                   y <- [-1,0,1],
                   z <- [-1,0,1],
                   x /= 0 || y /= 0 || z /= 0]

adjs4 :: [Point4]
adjs4 = [V4 x y z w | x <- [-1,0,1],
                      y <- [-1,0,1],
                      z <- [-1,0,1],
                      w <- [-1,0,1],
                      x /= 0 || y /= 0 || z /= 0 || w /= 0]

dayInput3 :: String -> Space3
dayInput3 = M.fromList
            . mapMaybe (\(p, c) -> if c == '#' then Just (p,True) else Nothing)
            . concatMap idxn
            . idx
            . lines
    where idx = zip [1..]
          idxn (x,s) = zipWith (\y c -> (V3 x y 1,c)) [1..] s

dayInput4 :: String -> Space4
dayInput4 = M.fromList
            . mapMaybe (\(p, c) -> if c == '#' then Just (p,True) else Nothing)
            . concatMap idxn
            . idx
            . lines
    where idx = zip [1..]
          idxn (x,s) = zipWith (\y c -> (V4 x y 1 1,c)) [1..] s