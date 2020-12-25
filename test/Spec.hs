module Main where

import Test.Hspec
import Days
import Data.Maybe (fromJust, isJust)
import Control.Monad (when)
import System.Directory


main :: IO ()
main = hspec $ describe "Advent of Code 2020" $ mapM_ testSolution tests

{-| 
  A list of the examples given with their answers. They're given
  in the following format:
   
  (Day, 
    (solution for part a, example answer), 
    (solution for part b, example answer)
  )
-}
tests :: [(Int, Maybe (String -> String, String), Maybe (String -> String, String))]
tests = [
          (1,
            Just (show . day01a, "514579" ), 
            Just (show . day01b, "241861950" ))
         ,(2,
            Just (show . day02a, "2" ),
            Just (show . day02b, "1" ))
         ,(3,
            Just (show . day03a, "7" ),
            Just (show . day03b, "336" ))
         ,(4,
            Just (show . day04a, "2" ),
            Just (show . day04b, "2" ))
         ,(5,
            Just (show . day05a, "820" ),
            Nothing )  -- No good test case
         ,(6, 
            Just (show . day06a, "11" ),
            Just (show . day06b, "6" ))
         ,(7, 
            Just (show . day07a, "4" ),
            Just (show . day07b, "32" ))
         ,(8, 
            Just (show . day08a, "5" ),
            Just (show . day08b, "8" ))
         ,(9, 
            Just (show . day09a 5, "127" ),
            Just (show . day09b 5, "62" ))
         ,(10,
            Just (show . day10a, "220" ),
            Just (show . day10b, "19208" ))
         ,(11,
            Just (show . day11a, "37" ),
            Just (show . day11b, "26" ))
         ,(12,
            Just (show . day12a, "25" ),
            Just (show . day12b, "286" ))
         ,(13,
            Just (show . day13a, "295" ),
            Just (show . day13b, "1068781" ))
         ,(14,
            Just (show . day14a, "165" ),
            Just (show . day14b, "208" ))
         ,(15,
            Just (show . day15a, "436" ),
            Just (show . day15b, "175594" ))
         ,(16,
            Just (show . day16a, "71" ),
            Nothing )  -- No good test case
         ,(17,
            Just (show . day17a, "112" ),
            Just (show . day17b, "848" ))
         ,(18,
            Just (show . day18a, "13632" ),
            Just (show . day18b, "23340" ))
         ,(19,
            Just (show . day19a, "2" ),
            Just (show . day19b, "12" ))
         ,(20,
            Just (show . day20a, "20899048083289" ),
            Just (show . day20b, "273" ))
         ,(21,
            Just (show . day21a, "5" ),
            Just (       day21b, "mxmxvkd,sqjhc,fvjkl" ))
        --  ,(22,
        --     Just (show . day22a, "165" ),
        --     Just (show . day22b, "208" ))
        --  ,(23,
        --     Just (show . day23a, "165" ),
        --     Just (show . day23b, "208" ))
        --  ,(24,
        --     Just (show . day24a, "165" ),
        --     Just (show . day24b, "208" ))
        --  ,(25,
        --     Just (show . day25a, "165" ),
        --     Just (show . day25b, "208" ))
        ]

-- | Runs the tests a given entry in the tests list.
testSolution :: (Int, Maybe (String -> String, String), Maybe (String -> String, String)) -> Spec
testSolution (n, a, b) = 
    context ("Day " ++ show n) $ do
        when (isJust a) $
          let (partA, ansA) = fromJust a
              in (it "Part A" $ do 
                    ex <- readEx "a" n
                    partA ex `shouldBe` ansA)
        
        when (isJust b) $
          let (partB, ansB) = fromJust b
              in (it "Part B" $ do 
                    ex <- readEx "b" n
                    partB ex `shouldBe` ansB)
        
            

-- | Reads the example input for a given day.
readEx :: String -> Int -> IO String
readEx part day = do
    e <- doesFileExist ("examples/" ++ prefix ++ show day ++ ".txt")
    if e then readFile $ "examples/" ++ prefix ++ show day ++ ".txt"
    else do
        e <- doesFileExist ("examples/" ++ prefix ++ show day ++ part ++ ".txt")
        if e then readFile ("examples/" ++ prefix ++ show day ++ part ++ ".txt")
        else fail "example file not found"
    where prefix | day < 10  = "0"
                 | otherwise = ""

noTest :: String -> String
noTest _ = "NO TEST"