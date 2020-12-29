{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Day22 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq(Empty),  Seq((:|>),(:<|)))
import Data.Functor
import Control.Applicative
import Parse
import Common
import Control.Monad.State.Strict (withState, runState, evalState, MonadState(get, put), modify, gets, State)
import Data.Sequence.Lens (viewL)
import Control.Lens (set, over, _1, _2)
import Control.Monad.Identity (Identity(Identity))
import Data.List.Split (splitOn)
import Data.Foldable (Foldable(toList))
import Data.Maybe (isJust)
import Debug.Trace (traceShow)
import Control.Monad (guard)

data Player = P1 | P2
type Decks = (Seq Int, Seq Int)
type Deck = Seq Int

day22a :: String -> Int
day22a = solveA . dayInput

solveA :: Decks -> Int
solveA = calculateScore . snd . uncurry gameA

calculateScore :: Seq Int -> Int
calculateScore cs = sum $ zipWith (*) scoreCard $ toList cs
    where scoreCard = enumFromThen (length cs) (length cs - 1)

day22b :: String -> Int
day22b = solveB . dayInput

solveB :: Decks -> Int
solveB = calculateScore . snd . uncurry gameB

gameA :: Deck -> Deck -> (Player, Deck)
gameA = gameWith $ \_ _ -> Nothing 

gameB :: Deck -> Deck -> (Player, Deck)
gameB = gameWith $ \(x:<|xs) (y:<|ys) -> do
    xs' <- takeExactly x xs
    ys' <- takeExactly y ys
    pure $ fst (gameB xs' ys')

gameWith :: (Deck -> Deck -> Maybe Player) -> Deck -> Deck -> (Player, Deck)
gameWith f = gameWith' f S.empty 

gameWith' :: (Deck -> Deck -> Maybe Player) -> Set (Deck,Deck) -> Deck -> Deck -> (Player, Deck)
gameWith' _ _    xs    Empty = (P1,xs)
gameWith' _ _    Empty ys    = (P2,ys)
gameWith' f seen d@(x:<|xs) d'@(y:<|ys)
    | (d,d') `S.member` seen = (P1, d)
    | otherwise = let winner = case f d d' of 
                        Nothing -> if x > y then P1 else P2
                        Just p  -> p
                  in case winner of
                    P1 -> gameWith' f seen' (xs:|>x:|>y) ys
                    P2 -> gameWith' f seen' xs (ys:|>y:|>x)
    where seen' = S.insert (d,d') seen

takeExactly :: Int -> Seq a -> Maybe (Seq a)
takeExactly n xs = Seq.take n xs <$ guard (Seq.length xs >= n) 

dayInput :: String -> Decks
dayInput input = (d1,d2)
    where [p1,p2] = splitOn "\n\n" input
          d1 = Seq.fromList $ unsafeParse rule p1
          d2 = Seq.fromList $ unsafeParse rule p2

rule :: Parsec String () [Int]
rule = do
    P.string "Player "
    P.oneOf "12"
    P.string ":"
    P.newline
    parseLines1 parseInt