{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Day15 where

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Foldable (for_, traverse_, Foldable(foldl'))
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed.Mutable (MVector)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_)
import Control.Monad.State.Strict (runState, execStateT, put, StateT(..), gets,get, evalStateT)

day15a :: String -> Int
day15a = solveA . dayInput

solveA :: [Int] -> Int
solveA = snd . speakN 2020

startingNumbers :: [Int] -> IntMap Int
startingNumbers = IM.fromList . flip zip [1..]

speakN :: Int -> [Int] -> (IntMap Int, Int)
speakN n sn = foldl' speak (startingNumbers sn, l) [length sn + 1 .. n]
    where l = last sn

speak :: (IntMap Int, Int) -> Int -> (IntMap Int, Int)
speak (ns, prev) n = case last of
    Just l  -> (ns', n-1-l)
    Nothing -> (ns', 0)
    where last = ns IM.!? prev
          ns'  = IM.insert prev (n-1) ns

vartNumbers :: Int -> [Int] -> Vector Int
vartNumbers n sn = V.replicate n (-1) V.// zip [1..] sn

veakN :: Int -> [Int] -> (Vector Int, Int)
veakN n sn = foldl' veak (vartNumbers n sn, l) [length sn + 1 .. n]
    where l = last sn

veak :: (Vector Int, Int) -> Int -> (Vector Int, Int)
veak (vs, prev) n
    | l /= -1   = (vs', n-1-l)
    | otherwise = (vs', 0)
    where l = vs V.! prev
          vs' =  vs V.// [(prev, n-1)]


-- meakN :: Int -> [Int] -> (Int, Int)
-- meakN n xs = runST $ flip evalStateT (0,head xs) $ do
--     v <- MV.replicate n 0
--     forM_ (zip [0..] xs) (\(i,y) -> (,(i+1,y)) <$> MV.write v i y)
--     forM_ [length xs + 1 .. n] $ StateT $ \(!i,!x) -> do
--         a
--     get

meakN :: Int -> [Int] -> StateT (Int, Int) (ST s) ()
meakN n xs = do
    v <- MV.replicate n 0
    forM_ (zip [0..] xs) (\(i,y) -> (,(i+1,y)) <$> MV.write v i y)
    meakN' v n

meakN' :: MVector s Int -> Int -> StateT (Int, Int) (ST s) ()
meakN' v n = whileM_ (gets ((< n) . snd)) $ meak v

meak :: MVector s Int -> StateT (Int, Int) (ST s)  ()
meak v = do
    lastSaid <- gets snd
    n <- gets fst
    pOcc <- MV.read v lastSaid
    MV.write v n lastSaid
    let says | pOcc == 0 = 0
             | otherwise = n-pOcc
    put (n+1, says)


day15b :: String -> Int
day15b = solveB . dayInput

main :: IO ()
main = print $ solveB [0,3,6]

solveB :: [Int] -> Int
solveB = snd . speakN 30000000

dayInput :: String -> [Int]
dayInput = map read . splitOn ","