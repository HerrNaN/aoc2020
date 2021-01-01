module Day23 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.CircularList as C
import qualified Data.Text as T
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.Set (Set)
import Data.CircularList (CList)
import Data.Vector.Unboxed.Mutable (MVector)

import Data.Functor
import Control.Applicative
import Parse
import Common
import Data.Char (intToDigit, digitToInt)
import Control.Monad (forM)
import Data.Maybe (fromJust)
import Control.Monad.State.Strict (execStateT, forM_, StateT(StateT))
import Control.Monad.ST.Strict (runST, ST)
import Data.Vector.Unboxed (Vector)

day23a :: String -> String
day23a = solveA . dayInput

solveA :: [Int] -> String
solveA xs = map intToDigit $ take 8 $ fromGameState res 1
    where res = runST $ do
            v <- initialize xs 9
            moveN 9 100 v
            V.freeze v          

type GameState s = MVector s Int

initialize :: [Int] -> Int -> ST s (GameState s)
initialize xs n = do
    let expanded = xs ++ [10..n]
    v <- MV.replicate (n+1) 0
    forM_ (zip expanded (tail expanded)) $ \(i,n) -> do
        MV.write v i n
    MV.write v (last expanded) (head expanded)
    MV.write v 0 (head expanded)
    return v

fromGameState :: Vector Int -> Int -> [Int]
fromGameState v pos = x : fromGameState v x
    where x = v V.! pos

move :: Int -> GameState s -> ST s ()
move nCups v = do
    curr <- MV.read v 0
    p1   <- MV.read v curr
    p2   <- MV.read v p1
    p3   <- MV.read v p2
    nextCurr <- MV.read v p3
    let destCup = dest nCups curr (p1,p2,p3)
    postDest <- MV.read v destCup
    MV.write v curr nextCurr
    MV.write v 0 nextCurr
    MV.write v destCup p1
    MV.write v p3 postDest

dest :: Int -> Int -> (Int, Int, Int) -> Int
dest nCups n (x,y,z)
    | nm1 /= x && nm1 /= y && nm1 /= z = nm1
    | nm2 /= x && nm2 /= y && nm2 /= z = nm2
    | nm3 /= x && nm3 /= y && nm3 /= z = nm3
    | otherwise = nm4
    where nm1 = if n   == 1 then nCups else n-1
          nm2 = if nm1 == 1 then nCups else nm1-1
          nm3 = if nm2 == 1 then nCups else nm2-1
          nm4 = if nm3 == 1 then nCups else nm3-1

moveN :: Int -> Int -> GameState s -> ST s ()
moveN nCups n v = forM_ [1..n] (\_ -> move nCups v)

day23b :: String -> Int
day23b = solveB . dayInput

solveB :: [Int] -> Int
solveB xs = runST $ do
    v <- initialize xs 1000000
    moveN 1000000 10000000 v
    p1 <- MV.read v 1
    p2 <- MV.read v p1
    return (p1*p2)

dayInput :: String -> [Int]
dayInput = map digitToInt . T.unpack . T.strip . T.pack