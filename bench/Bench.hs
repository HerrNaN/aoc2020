{-# LANGUAGE RankNTypes #-}
module Main where

import Criterion.Main

import qualified Criterion as C
import Days
import Input
import Advent (Part)
import Advent.Types (Part(..))
import Control.Monad (forM)

main :: IO ()
main = benches >>= Criterion.Main.defaultMain

benches :: IO [Benchmark]
benches = mapM (\(d,p,f) -> benchDayPart d p f)
    [
    --     (1, "A", toInteger . day01a)
    --    ,(1, "B", toInteger . day01b)
    --    ,(2, "A", toInteger . day02a)
    --    ,(2, "B", toInteger . day02b)
    --    ,(3, "A", toInteger . day03a)
    --    ,(3, "B", toInteger . day03b)
    --    ,(4, "A", toInteger . day04a)
    --    ,(4, "B", toInteger . day04b)
    --    ,(5, "A", toInteger . day05a)
    --    ,(5, "B", toInteger . day05b)
    --    ,(6, "A", toInteger . day06a)
    --    ,(6, "B", toInteger . day06b)
    --    ,(7, "A", toInteger . day07a)
    --    ,(7, "B", toInteger . day07b)
    --    ,(8, "A", toInteger . day08a)
    --    ,(8, "B", toInteger . day08b)
    --    ,(9, "A", toInteger . day09a 25)
    --    ,(9, "B", toInteger . day09b 25)
    --    ,(10, "A", toInteger . day10a)
    --    ,(10, "B", toInteger . day10b)
    --    ,(11, "A", toInteger . day11a)
    --    ,(11, "B", toInteger . day11b)
    --    ,(12, "A", toInteger . day12a)
    --    ,(12, "B", toInteger . day12b)
    --    ,(13, "A", day13a)
    --    ,(13, "B", day13b)
    --    ,(14, "A", day14a)
    --    ,(14, "B", day14b)
    --    ,(15, "A", toInteger . day15a)
       (15, "B", toInteger . day15b)
    --    ,(16, "A", toInteger . day16a)
    --    ,(16, "B", toInteger . day16b)
    --    ,(17, "A", day17a)
    --    ,(17, "B", day17b)
    --    ,(18, "A", day18a)
    --    ,(18, "B", day18b)
    --    ,(19, "A", day19a)
    --    ,(19, "B", day19b)
    --    ,(20, "A", day20a)
    --    ,(20, "B", day20b)
    --    ,(21, "A", day21a)
    --    ,(21, "B", day21b)
    --    ,(22, "A", day22a)
    --    ,(22, "B", day22b)
    --    ,(23, "A", day23a)
    --    ,(23, "B", day23b)
    --    ,(24, "A", day24a)
    --    ,(24, "B", day24b)
    --    ,(25, "A", day25a)
    --    ,(25, "B", day25b)
    ]

benchDayPart :: forall a. Integer -> String -> (String -> a) -> IO Benchmark 
benchDayPart day part f = do
    input <- getInput day
    return (C.bench ("Day " ++ show day ++ " Part " ++ part) (C.whnf f input))