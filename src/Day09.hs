{-# LANGUAGE RecordWildCards #-}
module Day09 where

import qualified Text.Parsec as P
import           Text.Parsec (Parsec)
import           Parse
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import qualified Data.Map.Strict as Map
import           Data.Map (Map)
import           Data.Sequence.Internal (Seq((:<|),(:|>)))
import Debug.Trace
import Data.Maybe (isNothing)
import Control.Applicative (Applicative(liftA2))

day09a :: Int -> String -> Int
day09a n = solveA n . dayInput

solveA :: Int -> [Int] -> Int
solveA size xs = case runCheck $ initChecker size xs of
    Just (_, n)  -> n
    Nothing -> error "no invalid value found"

initChecker :: Int -> [Int] -> Checker
initChecker n xs = C
    { cPreamble = (Seq.fromList $ take n xs, Map.fromListWith (+) $ take n $ zip xs $ repeat 1)
    , cList     = Seq.fromList xs
    , cPc       = n
    }

runCheck :: Checker -> Maybe (Int, Int)
runCheck c@C{..}
    | done      = Nothing
    | not found = Just (cPc, Seq.index cList cPc)
    | otherwise = runCheck c'
    where done  = isNothing $ Seq.lookup cPc cList
          found = isValid c
          c'    = step c

foldlCheck 
    :: (Int -> Bool) 
    -> ((Bool, Int) 
    -> Int -> Int) 
    -> (Bool, Int) 
    -> [Int] 
    -> (Bool, Int)
foldlCheck p f = foldl f'
    where f' a b = (p $ f a b, f a b)
          



sumOf2In :: Int -> Preamble -> Bool
sumOf2In _ (Seq.Empty, _) = False
sumOf2In x (i :<| is,  m)
    | Map.notMember (x-i) m = sumOf2In x (is, m)
    | x-i == i  = n > 1 || sumOf2In x (is, m)
    | otherwise = True
    where n = m Map.! (x-i)


incAt :: Int -> Map Int Int -> Map Int Int
incAt n = Map.insertWith (+) n 1

decAt :: Int -> Map Int Int -> Map Int Int
decAt n m = if m Map.! n == 1
                then Map.delete n m 
            else Map.adjust (flip (-) 1) n m

type Preamble = (Seq Int, Map Int Int)
data Checker = C
    { cPreamble :: Preamble
    , cList     :: Seq Int
    , cPc       :: Int
    } deriving (Show)

updateP :: Int -> Preamble -> Preamble
updateP n' (n :<| s, m)
    = (s :|> n', incAt n' (decAt n m))

step :: Checker -> Checker
step c@C{..} = c{cPreamble=p,cPc=cPc+1}
    where p = updateP (Seq.index cList cPc) cPreamble

isValid :: Checker -> Bool
isValid C{..} = sumOf2In x cPreamble
    where x = Seq.index cList cPc

day09b :: Int -> String -> Int
day09b n = solveB n . dayInput

solveB :: Int -> [Int] -> Int
solveB n xs = case weakness $ initChecker n xs of
    Just x  -> x
    Nothing -> error "no weakness found"


weakness :: Checker -> Maybe Int
weakness c@C{..} = do 
    (i,n) <- runCheck c
    findWeakness n $ Seq.take i cList

findWeakness :: Int -> Seq Int -> Maybe Int
findWeakness x s = findWeakness' x s 2

findWeakness' :: Int -> Seq Int -> Int -> Maybe Int
findWeakness' x s n
    | n > Seq.length s = Nothing
    | sum' == x = Just $ min+max
    | sum' > x  = findWeakness x (Seq.drop 1 s)
    | otherwise = findWeakness' x s (n+1)
    where (min, max) = (minimum s', maximum s')
          s' = Seq.take n s
          sum' = sum s'



dayInput :: String -> [Int]
dayInput input = case parse ints input of
    Right is -> is
    Left e   -> error $ show e

ints :: Parsec String () [Int]
ints = P.sepEndBy1 parseInt P.newline
