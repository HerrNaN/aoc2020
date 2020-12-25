module Day21 where

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
import Data.List (sortBy, sort, intercalate)
import Data.Ord (comparing)

type Allergen = String
type Ingredient = String


reduceAll :: Map Allergen [Set Ingredient] -> Map Allergen (Set Ingredient)
reduceAll = M.map reduce

reduce :: [Set Ingredient] -> Set Ingredient
reduce [p]    = p
reduce (p:ps) = foldl S.intersection p ps

eliminateAll :: Map Allergen (Set Ingredient) -> Map Allergen Ingredient
eliminateAll m = eliminateAll' (M.empty, m)

eliminateAll' :: (Map Allergen Ingredient, Map Allergen (Set Ingredient)) -> Map Allergen Ingredient
eliminateAll' (am, rm)
    | M.null rm = am
    | otherwise = eliminateAll' $ eliminate (am, rm)

eliminate :: (Map Allergen Ingredient, Map Allergen (Set Ingredient)) -> (Map Allergen Ingredient, Map Allergen (Set Ingredient))
eliminate (am, rm) = (am', rm')
    where known = M.elems am
          erm = M.map (eliminate' known) rm
          am' = M.union (M.map (head . S.toList) $ M.filter ((==1) . S.size) erm) am
          rm' = M.filter ((>1) . S.size) erm

eliminate' :: [Ingredient] -> Set Ingredient -> Set Ingredient
eliminate' is iSet = foldl (flip S.delete) iSet is

day21a :: String -> Int
day21a = solveA . dayInput

solveA ::  [([Ingredient], [Allergen])] -> Int
solveA rs = sum $ map (`count` is) safeIs
    where asm = toAllergenMap rs
          is  = concatMap fst rs
          iSet = S.fromList is
          safeIs = S.toList $ iSet S.\\ S.fromList (M.elems $ eliminateAll $ reduceAll asm)

toAllergenMap :: [([Ingredient], [Allergen])] -> Map Allergen [Set Ingredient]
toAllergenMap [] = M.empty 
toAllergenMap ((is,as):ias) = foldl (\m' a -> M.insertWith (++) a [iSet] m') m as
    where m   = toAllergenMap ias
          iSet = S.fromList is

day21b :: String -> String
day21b = solveB . dayInput

solveB ::  [([Ingredient], [Allergen])] -> String
solveB rs = intercalate "," $ map snd $ sortBy (comparing fst) as
    where asm = toAllergenMap rs
          is  = concatMap fst rs
          as  = M.toList $ eliminateAll $ reduceAll asm


dayInput :: String -> [([Ingredient], [Allergen])]
dayInput = unsafeParse (parseLines1 rule)

rule :: Parsec String () ([Ingredient], [Allergen])
rule = do
    is <- P.manyTill word (P.string "(contains ")
    as <- P.many (word <* P.optional (P.string " "))
    return (is,as)


word :: Parsec String () String
word = P.manyTill P.letter (P.oneOf " ,)")