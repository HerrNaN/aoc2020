{-# LANGUAGE RecordWildCards #-}
module Day08 where

import           Parse
import           Text.Parsec ((<|>),  Parsec )
import qualified Text.Parsec   as P
import qualified Data.Sequence as Seq
import           Data.Set.Internal ( Set )
import qualified Data.Set as Set
import           Data.Functor (($>))
import           Data.Sequence.Internal (Seq((:<|)), (><))
import           Data.Either ()
import           Data.Maybe
import           Data.Bifunctor (Bifunctor(first))
import           Control.Monad.State

day08a :: String -> Int
day08a = solveA . dayInput'

solveA :: Program' -> Int
solveA p = case s of
            Loop' n -> n
            _       -> error "didn't find a loop"
    where s = evalState run $ Env{eMemory=p,ePC=0,eAcc=0,eRunLines=Set.empty}

runProg :: ProgramState -> ExitCode
runProg s = case step s of
    Left code -> code
    Right s'  -> runProg s'

step :: ProgramState -> Either ExitCode ProgramState
step (acc, pis, xs, ys@((n, i) :<| _))
    | terminate = Left $  Exit Term acc'
    | foundLoop = Left $  Exit Loop acc
    | otherwise = Right (acc', pis', xs', ys') 
    where foundLoop = Set.member n pis
          acc' = updateAcc acc i
          pis' = Set.insert n pis
          (d, n')    = move i
          terminate  = d == Down && (Seq.length xs + Seq.length ys) < n + n'
          (xs', ys') = case d of
              -- Down xs+n' ys-n'
              Down -> (xs >< Seq.take n' ys, Seq.drop n' ys)
              -- Up   xs-n' ys+n'
              Up -> (dropR n' xs, takeR n' xs >< ys)

trySwitch :: ProgramState -> ExitCode
trySwitch s@(acc, pis, xs, (n, i) :<| ys')
    | fst i == Acc = case step s of
        Left code -> code
        Right s'  -> trySwitch s'
    | otherwise    = case runProg (acc, pis, xs, (n, (op, n')) :<| ys') of
        Exit Loop _ -> case step s of
            Left  code -> code
            Right s'   -> trySwitch s'
        code -> code
    where 
        (op, n') = first opSwitch i 

opSwitch :: Op -> Op
opSwitch Nop = Jmp
opSwitch Jmp = Nop

dropR :: Int -> Seq a -> Seq a
dropR n s = Seq.take (length s - n) s

takeR :: Int -> Seq a -> Seq a
takeR n s = Seq.drop (length s - n) s

move :: Instruction -> (Dir, Int)
move (Jmp, n) = if n < 0 then (Up, -n) else (Down, n)
move _             = (Down, 1)

updateAcc :: Int -> Instruction -> Int
updateAcc n (op, x)
    | op == Acc = n + x
    | otherwise = n

day08b :: String -> Int
day08b = solveB . dayInput

solveB :: Program -> Int
solveB p = case trySwitch (0, Set.empty, Seq.empty, p) of
    Exit Loop n -> error $ "program entered infinite loop with value " ++ show n
    Exit Term n -> n

type ProgramState = (Int, Set Int, Program, Program)
data ExitCode = Exit ExitType Int
data ExitType = Term | Loop
data Dir = Up | Down deriving (Show, Eq)
data Op = Nop | Acc | Jmp deriving (Show, Eq)
type Instruction = (Op, Int)
type Program = Seq (Int, Instruction)
type Program' = Seq Instruction
data Status = Term' Int | Loop' Int | Running' deriving (Eq)

data Env = Env 
    { eMemory :: Seq Instruction
    , ePC :: Int
    , eAcc :: Int
    , eRunLines :: Set Int
    }

type Prog = State Env Status

run :: Prog
run = do
    pc <- gets ePC
    memSize <- gets (Seq.length . eMemory)
    runLines <- gets eRunLines
    if pc >= memSize
        then gets (Term' . eAcc)
    else if Set.member pc runLines
        then gets (Loop' . eAcc)
    else do
        gets (line pc) >>= step'
        modify (addLine pc)
        run
    where addLine n e@Env{..} = e{eRunLines = Set.insert n eRunLines}

incPc :: Prog
incPc = withState incPc' $ return Running'
    where incPc' e@Env{..} = e{ePC = ePC + 1}

step' :: Instruction -> Prog
step' (op, n)
    | op == Acc = withState acc incPc
    | op == Jmp = withState jmp $ return Running'
    | otherwise = incPc
    where acc e@Env{..} = e{eAcc = eAcc + n}
          jmp e@Env{..} = e{ePC  = ePC +  n} 

line :: Int -> Env -> Instruction
line n = fromJust . Seq.lookup n . eMemory

dayInput' :: String -> Program'
dayInput' = Seq.fromList . unsafeParse instructions

dayInput :: String -> Program
dayInput = Seq.fromList . zip [1..] . unsafeParse instructions

instructions :: Parsec String () [Instruction]
instructions = P.sepEndBy1 instruction P.newline

instruction :: Parsec String () Instruction
instruction = do
    op <- P.try (P.string "nop" $> Nop) <|>
            P.try (P.string "acc" $> Acc) <|>
            P.string "jmp" $> Jmp
    P.space
    int <- signedInt
    return (op, int)

signedInt :: Parsec String () Int
signedInt = do
    sign <- P.string "-" <|> (P.string "+" $> "")
    digs <- P.many1 P.digit
    return $ read $ sign ++ digs
