module Main where

import Test.Hspec


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
tests = []

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
readEx day = readFile $ "examples/day" ++ day' ++ ".txt"
    where day' = if day < 10 then "0" ++ show day else show day