module DayXX where

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

dayXXa :: String -> b
dayXXa = solveA . dayInput

solveA :: a -> b
solveA = error "not implemented"

dayXXb :: String -> b
dayXXb = solveB . dayInput

solveB :: a -> b
solveB = error "not implemented"

dayInput :: String -> a
dayInput = unsafeParse rule

rule :: Parsec String () a
rule = error "not implemented"