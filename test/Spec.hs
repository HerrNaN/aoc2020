module Main where

import Test.Hspec
import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03


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
          (1, (show . D01.solveA, "514579"), (show . D01.solveB, "241861950"))
         ,(2, (show . D02.solveA, "2"     ), (show . D02.solveB, "1"        ))
         ,(3, (show . D03.solveA, "7"     ), (show . D03.solveB, "336"      ))
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