{-# LANGUAGE OverloadedStrings          #-}

module AOC5 where

import           Text.Parsec                    ( digit
                                                , many1
                                                , parse
                                                , skipMany
                                                , space
                                                , string
                                                , (<|>)
                                                , sepBy
                                                )
import           Text.Parsec.ByteString         ( Parser
                                                , parseFromFile
                                                )
import           Data.Array                     ( Array
                                                , (!)
                                                , (//)
                                                , listArray
                                                )
import           Data.Maybe                     ( isJust )
import           Control.Monad.State.Strict     ( State
                                                , get
                                                , gets
                                                , put
                                                , execState
                                                , modify
                                                )

number :: Parser Int
number = read <$> many1 digit

parseOp = number `sepBy` (string ",")

convert :: [Int] -> Array Int Int
convert xs = listArray (0, (length xs) - 1) xs

type Value = Int
type Address = Int
data Machine = Machine { memory :: Array Address Value, opCode :: Address, input :: Value, output :: Value }
type MachineState = State Machine

buildMachine :: [Value] -> Machine
buildMachine input = Machine { memory = (convert input), opCode = 0, input = 0, output = 0 }

setup :: Value -> Value -> Machine -> Machine
setup input' output' m =
    Machine { memory = (memory m), opCode = 0, input = input', output = output' }


solution1 :: IO Value
solution1 = do
    ops <- parseFromFile parseOp "AOC2.input"
    let input = setup 12 2 . buildMachine <$> ops
    case input of
        --Right m -> return . (! 0) . memory $ execState runUntilHalt m
        Right _ -> error "no solution yet"
        Left  e -> error $ show e


solution2 :: IO Value
solution2 = do
    ops <- parseFromFile parseOp "AOC2.input"
    case ops of
        Right _ -> error "no solution yet"
        Left  e -> error $ show e

runUntilHalt :: MachineState ()
runUntilHalt = do
    opAddr    <- gets opCode
    operation <- loadMemory opAddr
    case operation of
        1  -> opAdd >> runUntilHalt
        2  -> opMul >> runUntilHalt
        3  -> readInput >> runUntilHalt
        4  -> writeOutput >> runUntilHalt
        99 -> return ()
        x  -> error $ "unknown opcode: " ++ show x

readInput :: MachineState ()
readInput = do
  o <- gets opCode
  dest <- loadMemory (o + 1)
  input' <- gets input
  store dest input'
  modify (\s -> s {opCode = o + 2})

writeOutput :: MachineState ()
writeOutput = do
  o <- gets opCode
  val <- loadMemory (o + 1)
  modify (\s -> s {opCode = o + 2, output = val})

opAdd :: MachineState ()
opAdd = mathOp (+)
opMul :: MachineState ()
opMul = mathOp (*)

mathOp :: (Value -> Value -> Value) -> MachineState ()
mathOp op = do
    o  <- gets opCode
    a1 <- loadIndirect (o + 1)
    a2 <- loadIndirect (o + 2)
    storeIndirect (o + 3) (a1 `op` a2)
    m' <- gets memory
    modify (\s -> s { memory = m', opCode = o + 4 })

loadMemory :: Address -> MachineState Value
loadMemory x = do
    m <- gets memory
    return $ m ! x

loadIndirect :: Address -> MachineState Value
loadIndirect x = do
    m <- gets memory
    let ref = m ! x
    return $ m ! ref

store :: Address -> Value -> MachineState ()
store target v = do
    m <- gets memory
    let m' = m // [(target, v)]
    modify (\s -> s { memory = m'})

storeIndirect :: Address -> Value -> MachineState ()
storeIndirect x v = do
    m      <- gets memory
    target <- loadMemory x
    let m' = m // [(target, v)]
    modify (\s -> s { memory = m'})
