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
import Control.Monad.State.Strict (evalState, MonadState(put), modify, gets, State)
import Data.Sequence.Lens (viewL)
import Control.Lens (set, over, _1, _2)
import Control.Monad.Identity (Identity(Identity))
import Data.List.Split (splitOn)
import Data.Foldable (Foldable(toList))

type CombatGame = State Decks Int
type Decks = (Seq Int, Seq Int)

day22a :: String -> Int
day22a = solveA . dayInput

solveA :: Decks -> Int
solveA = evalState playGame

winner :: Decks -> Maybe (Seq Int)
winner (Empty, cs   ) = Just cs
winner (cs   , Empty) = Just cs
winner _ = Nothing

playGame :: CombatGame
playGame = do
    wcs <- gets winner
    case wcs of
        Just cs -> return $ calculateScore cs
        Nothing -> playRound

calculateScore :: Seq Int -> Int
calculateScore cs = sum $ zipWith (*) scoreCard $ toList cs
    where scoreCard = enumFromThen (length cs) (length cs - 1)

playRound :: CombatGame
playRound = do
    (c,c') <- gets topCards
    modify dropTopCards
    modify (updateCards (c,c'))
    playGame

updateCards :: (Int, Int) -> (Seq Int, Seq Int) -> (Seq Int, Seq Int)
updateCards (c,c') (cs,cs')
    | c >  c' = (cs:|>c:|>c',cs')
    | c' > c  = (cs, cs':|>c':|>c)
    | otherwise = error "equal cards!"

dropTopCards :: (Seq Int, Seq Int) -> (Seq Int, Seq Int)
dropTopCards (_:<|cs, _:<|cs') = (cs,cs')

topCards :: (Seq Int, Seq Int) -> (Int, Int)
topCards (c:<|_, c':<|_) = (c,c')

day22b :: String -> Int
day22b = solveB . dayInput

solveB :: Decks -> Int
solveB = error "not implemented"

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