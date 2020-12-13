module Main where

import Test.Hspec
import Days
import Input


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
tests :: [(Int, (String -> String, String), (String -> String, String))]
tests = [
          (1, (show . day01a, "514579"), (show . day01b, "241861950"))
         ,(2, (show . day02a, "2"     ), (show . day02b, "1"        ))
         ,(3, (show . day03a, "7"     ), (show . day03b, "336"      ))
         ,(4, (show . day04a, "2"     ), (show . day04b, "2"        ))
         ,(5, (show . day05a, "820"   ), (       noTest, "NO TEST"  ))
         ,(6, (show . day06a, "11"    ), (show . day06b, "6"        ))
         ,(7, (show . day07a, "4"     ), (show . day07b, "32"       ))
         ,(8, (show . day08a, "5"     ), (show . day08b, "8"        ))
         ,(9, (show . day09a 5, "127" ), (show . day09b 5, "62"     ))
         ,(10,(show . day10a, "220"   ), (show . day10b, "19208"    ))
         ,(11,(show . day11a, "37"    ), (show . day11b, "26"       ))
         ,(12,(show . day12a, "25"    ), (show . day12b, "286"      ))
         ,(13,(show . day13a, "295"   ), (show . day13b, "1068788"  ))
        ]

-- | Runs the tests a given entry in the tests list.
testSolution :: (Int, (String -> String, String), (String -> String, String)) -> Spec
testSolution (n, (partA, ansA), (partB, ansB)) = 
    context ("Day " ++ show n) $ do
        it "Part A" $ do 
            ex <- readEx n
            partA ex `shouldBe` ansA
        it "Part B" $ do 
            ex <- readEx n
            partB ex `shouldBe` ansB

-- | Reads the example input for a given day.
readEx :: Int -> IO String
readEx day | day < 10  = readFile $ "examples/0" ++ show day ++ ".txt"
           | otherwise = readFile $ "examples/"  ++ show day ++ ".txt"

noTest :: String -> String
noTest _ = "NO TEST"