{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Day04 where

import qualified Text.Parsec as Parsec
import Parse
import Control.Applicative (Alternative((<|>)))
import Data.Maybe
import Data.Functor (($>))
import Debug.Trace (trace)

type Year = String
type Height = String
type Color = String
type ID = String
data Passport = Passport {
    byr :: Year,
    iyr :: Year,
    eyr :: Year,
    hgt :: Height,
    hcl :: Color,
    ecl :: Color,
    pid :: ID,
    cid :: Maybe ID
} deriving (Show)

createPassport :: [(String, String)] -> Passport
createPassport kvs = createPassport' kvs Passport{
    byr = "",
    iyr = "",
    eyr = "",
    hgt = "",
    hcl = "",
    ecl = "",
    pid = "",
    cid = Nothing
}

createPassport' :: [(String, String)] -> Passport -> Passport
createPassport' [] p = p
createPassport' (("byr", v):kvs) p = createPassport' kvs $ p {byr = v}
createPassport' (("iyr", v):kvs) p = createPassport' kvs $ p {iyr = v}
createPassport' (("eyr", v):kvs) p = createPassport' kvs $ p {eyr = v}
createPassport' (("hgt", v):kvs) p = createPassport' kvs $ p {hgt = v}
createPassport' (("hcl", v):kvs) p = createPassport' kvs $ p {hcl = v}
createPassport' (("ecl", v):kvs) p = createPassport' kvs $ p {ecl = v}
createPassport' (("pid", v):kvs) p = createPassport' kvs $ p {pid = v}
createPassport' (("cid", v):kvs) p = createPassport' kvs $ p {cid = Just v}

isValidA :: Passport -> Bool
isValidA p = 
    byr p /= "" &&
    iyr p /= "" &&
    eyr p /= "" &&
    hgt p /= "" &&
    hcl p /= "" &&
    ecl p /= "" &&
    pid p /= ""

isValidB :: Passport -> Bool
isValidB p = isValidA p &&
    byr p /= "!BAD!" && byr' >= 1920 && byr' <= 2002 &&
    iyr p /= "!BAD!" && iyr' >= 2010 && iyr' <= 2020 &&
    eyr p /= "!BAD!" && eyr' >= 2020 && eyr' <= 2030 &&
    hgt p /= "!BAD!" && hgtValid &&
    hcl p /= "!BAD!" &&
    ecl p /= "!BAD!" &&
    pid p /= "!BAD!"
    where byr' = read (byr p) :: Int
          iyr' = read (iyr p) :: Int
          eyr' = read (eyr p) :: Int
          hgtValid = case parse parseHeight (hgt p) of
                    Right (i, "cm") -> i >= 150 && i <= 193
                    Right (i, "in") -> i >= 59 && i <= 76

solveA :: String -> Int
solveA = solveA' . dayInput

solveA' :: [Passport] -> Int
solveA' = sum . map (fromEnum . isValidA)

solveB :: String -> Int
solveB = solveB' . dayInput

solveB' :: [Passport] -> Int
solveB' = sum . map (fromEnum . isValidB)

dayInput :: String -> [Passport]
dayInput input = case parse parsePassports input of
    Right ps -> ps
    Left e -> error $ show e

parsePassports :: Parsec.Parsec String () [Passport]
parsePassports = Parsec.sepBy1 parsePassport parseBlankLine

parsePassport :: Parsec.Parsec String () Passport
parsePassport = do 
    kvs <- Parsec.sepBy1 parseKVPair parseKVSep
    return $ createPassport kvs
    
parseKVSep :: Parsec.Parsec String () Char
parseKVSep = Parsec.notFollowedBy (Parsec.newline >> Parsec.eof) >>
                Parsec.notFollowedBy parseBlankLine *>
                (Parsec.space <|> Parsec.newline)

parseKVPair :: Parsec.Parsec String () (String, String)
parseKVPair = do 
    key <- parseKey
    Parsec.char ':'
    val <- Parsec.try (parseValue key <* Parsec.lookAhead (Parsec.space <|> Parsec.newline)) <|>
                (Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.space <|> Parsec.newline)) $> "!BAD!")
    return (key, val)

parseKey :: Parsec.Parsec String () String
parseKey = Parsec.try (Parsec.string "byr") <|>
           Parsec.try (Parsec.string "iyr") <|>
           Parsec.try (Parsec.string "eyr") <|>
           Parsec.try (Parsec.string "hgt") <|>
           Parsec.try (Parsec.string "hcl") <|>
           Parsec.try (Parsec.string "ecl") <|>
           Parsec.try (Parsec.string "pid") <|>
           Parsec.string "cid"

parseValue :: [Char] -> Parsec.Parsec String () String
parseValue "byr" = parseYear
parseValue "iyr" = parseYear
parseValue "eyr" = parseYear
parseValue "hgt" = parseHeightString
parseValue "hcl" = parseHairColor
parseValue "ecl" = parseEyeColor
parseValue "pid" = parseID
parseValue "cid" = parseID

parseYear :: Parsec.Parsec String () String
parseYear = Parsec.count 4 Parsec.digit

parseHeightString :: Parsec.Parsec String () String
parseHeightString = do
    (i, unit) <- parseHeight
    return $ show i ++ unit

parseHeight :: Parsec.Parsec String () (Int, String)
parseHeight = do
    i <- parseInt
    unit <- Parsec.string "cm" <|> Parsec.string "in"
    return (i, unit)

parseColor :: Parsec.Parsec String () String
parseColor = Parsec.try parseEyeColor <|> Parsec.try parseHairColor <|> Parsec.string "z"

parseEyeColor :: Parsec.Parsec String () String
parseEyeColor = 
    Parsec.try (Parsec.string "amb") <|>
    Parsec.try (Parsec.string "blu") <|>
    Parsec.try (Parsec.string "brn") <|>
    Parsec.try (Parsec.string "gry") <|>
    Parsec.try (Parsec.string "grn") <|>
    Parsec.try (Parsec.string "hzl") <|>
    Parsec.string "oth"

parseHairColor :: Parsec.Parsec String () String
parseHairColor = do
    Parsec.char '#'
    col <- Parsec.count 6 Parsec.hexDigit
    return $ "#" ++ col

parseID :: Parsec.Parsec String () String
parseID = Parsec.count 9 Parsec.digit

parseBlankLine :: Parsec.Parsec String () String
parseBlankLine = Parsec.notFollowedBy (Parsec.newline >> Parsec.eof) *> Parsec.count 2 Parsec.newline