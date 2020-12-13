module Day13 where

import Parse
import qualified Text.Parsec as P
import Text.Parsec (Parsec)
import Control.Applicative (Alternative((<|>)))
import Data.Functor ((<&>), ($>))
import Data.Maybe
import Data.Ord (comparing)
import Data.List

day13a :: String -> Integer
day13a = solveA . dayInput

solveA :: (Integer, [Maybe Integer]) -> Integer
solveA (arrive, bs) = b * w
    where (b, w) = minimumBy (comparing fst) 
                 . map (\x -> (x - (arrive `mod` x),x)) 
                 $ catMaybes bs

day13b :: String -> Integer
day13b = solveB . dayInput

solveB :: (Integer, [Maybe Integer]) -> Integer
solveB (_, bs)
    = chineseRemainder 
    . unzip 
    . map (\(r,m) -> (-r, fromJust m)) 
    . filter (isJust . snd) 
    $ zip [0..] bs

egcd :: Integer -> Integer -> (Integer, Integer)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b
 
modInv :: Integer -> Integer -> Integer
modInv a b = fst $ egcd a b
 
chineseRemainder :: ([Integer], [Integer]) -> Integer
chineseRemainder (residues, modulii) 
    = (`mod` modPI) 
    . sum 
    . zipWith (*) crtModulii 
    . zipWith (*) residues 
    $ zipWith modInv crtModulii modulii
    where
        modPI = product modulii
        crtModulii = map (modPI `div`) modulii

dayInput :: String -> (Integer, [Maybe Integer])
dayInput input = case parse busTable input of
                    Right x -> x
                    Left  e -> error $ show e
busTable :: Parsec String () (Integer, [Maybe Integer])
busTable = do
    i <- parseInt
    P.newline
    bls <- busLines
    return (toInteger i, bls)

busLines :: Parsec String () [Maybe Integer]
busLines = P.sepBy1 busLine (P.char ',')

busLine :: Parsec String () (Maybe Integer)
busLine = (P.try parseInt <&> Just . toInteger) <|> (P.char 'x' $> Nothing)