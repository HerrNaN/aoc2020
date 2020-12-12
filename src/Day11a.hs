{-# LANGUAGE LambdaCase #-}
module Day11a where

import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe
import Common ( countTrue, fixedPoint )
import Linear.V2
import Data.Ix ( Ix(inRange) )
import Data.Foldable (find)

day11a :: String -> Int
day11a = solveA . dayInput

solveA :: Seats -> Int
solveA sm = countTrue id $ fixedPoint (step 4 los) sm
    where los = lineOfSightA sm

day11b :: String -> Int
day11b = solveB . dayInput

solveB :: Seats -> Int
solveB sm = countTrue id $ fixedPoint (step 5 los) sm
    where los = lineOfSightB sm

type Point = V2 Int
type Seat = Bool
type Seats = Map Point Bool
type LOS = Map Point (Set Point)

lineOfSightA :: Seats -> LOS
lineOfSightA sm = M.mapWithKey (\p _ -> neighbourSet sm p) sm

neighbourSet :: Seats -> Point -> Set Point
neighbourSet sm p = S.fromList $ filter (`M.member` sm) $ map (+p) adjs

lineOfSightB :: Seats -> LOS
lineOfSightB sm = M.mapWithKey (\p _ -> losSet sm p) sm

losSet :: Seats -> Point -> Set Point
losSet sm p = S.fromList $ mapMaybe (losInDir sm p) adjs

losInDir :: Seats -> Point -> Point -> Maybe Point
losInDir sm p d = find (`M.member` sm)
                . takeWhile (all (inRange (1,100)))
                . tail
                $ iterate (+d) p

step :: Int -> LOS -> Seats -> Seats
step thr los ss = M.intersectionWith go los ss
    where go nbh = \case
            False -> not $ any (ss M.!) nbh
            True  -> countTrue (ss M.!) nbh < thr

adjs :: [Point]
adjs = [V2 x y | x <- [-1,0,1],
                y <- [-1,0,1],
                x /= 0 || y /= 0]

dayInput :: String -> Seats
dayInput = seats . lines

seats :: [String] -> Seats
seats = M.fromList
            . mapMaybe (\(p, c) -> if c == 'L' then Just (p,False) else Nothing)
            . concatMap idxn
            . idx 
    where idx = zip [1..]
          idxn (x,s) = zipWith (\y c -> (V2 x y,c)) [1..] s