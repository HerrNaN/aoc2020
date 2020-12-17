{-# LANGUAGE RecordWildCards #-}
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
import Linear.V
import Linear (V2(V2))
import Linear.V2
import Control.Lens

-- type Point3 = V3 Int
-- type Point4 = V4 Int
-- type Space = Map Point3 Bool
type Space n = V n Int
-- type Space3 = Map Point3 Bool
-- type Space4 = Map Point4 Bool
-- type Neighbours = Map (V3 Int) (Set (V3 Int))

day17a :: String -> Int
day17a = solveA . dayInput toV3

solveA :: Set (V3 Int) -> Int
solveA = S.size . (!! 6) . iterate step

day17b :: String -> Int
day17b = solveB . dayInput toV4

solveB :: Set (V4 Int) -> Int
solveB = S.size . (!! 6) . iterate step

step ::
    (Traversable t, Applicative t, Num a1, Num (t a1), Ord (t a1))
    => Set (t a1) -- Set of active cubes
    -> Set (t a1) -- Set of active cubes
step s = survive <> rise
    where survive = M.keysSet . M.filter (\n -> n == 2 || n == 3)
                        $ M.restrictKeys nbhm s
          rise = M.keysSet . M.filter (== 3)
                    $ M.withoutKeys nbhm s
          nbhm = activeNeighboursMap s

activeNeighboursMap ::
    (Ord (t a1), Traversable t, Applicative t, Num a2, Num a1, Num (t a1))
    => Set (t a1)    -- Set of active cubes
    -> Map (t a1) a2 -- Map of cube to its active neighbours
activeNeighboursMap s = M.unionsWith (+) $
    [ M.fromSet (const 1) (neighboursSet p)
    | p <- S.toList s ]

dayInput ::
    (Ord (t a), Traversable t, Applicative t, Num a, Num (t a))
    => (Int -> Int -> t a) -- Upscaling dimention function
    -> String              -- Input
    -> Set (t a)           -- Set of active cubes
dayInput f = S.fromList
            . catMaybes
            . concatMap idxn
            . zip [1..]
            . lines
    where idxn (x,s) = zipWith (\y c -> if c == '#' then Just (f x y) else Nothing) [1..] s

toV3 :: Num a => a -> a -> V3 a
toV3 x y = V3 x y 1

toV4 :: Num a => a -> a -> V4 a
toV4 x y = V4 x y 1 1

neighboursSet ::
    (Ord (t a), Traversable t, Applicative t, Num a, Num (t a))
    => t a
    -> Set (t a)
neighboursSet p = S.fromList
    [ p+d
    | d <- sequence (pure [-1,0,1])
    , d /= pure 0
    ]