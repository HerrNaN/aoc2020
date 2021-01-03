module Day25 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.Set (Set)
import Data.Functor
import Control.Applicative
import Parse
import Common
import Data.Foldable (find)
import Data.Maybe (fromJust)

day25a :: String -> Integer
day25a = solveA . dayInput

solveA :: (Integer,Integer) -> Integer
solveA (pk, pk') = transform ls pk'
    where ls = getLoopSize pk

day25b :: String -> Integer
day25b = solveB . dayInput

solveB :: (Integer,Integer) -> Integer
solveB = error "not implemented"

step :: Integer -> Integer -> Integer
step sn v = let v' = v * sn
            in v' `rem` 20201227

transform :: Int -> Integer -> Integer
transform ls sn = iterate (step sn) 1 !! ls

getLoopSize :: Integer -> Int
getLoopSize pk = fst $ fromJust $ find ((==pk) . snd) $ zip [0..] $ iterate (step 7) 1

dayInput :: String -> (Integer,Integer)
dayInput = unsafeParse rule

rule :: Parsec String () (Integer,Integer)
rule = do
    i <- parseInt
    P.newline 
    j <- parseInt
    return (toInteger i,toInteger j)