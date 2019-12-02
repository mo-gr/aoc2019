{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module AOC1 where

import           Text.Parsec            (digit, many1, parse, skipMany, space,
                                         string, (<|>))
import           Text.Parsec.ByteString (Parser, parseFromFile)

number :: Parser Integer
number = read <$> many1 digit

parseOp = error "no parser specified"

solution1 :: IO String
solution1 = do
  ops <- parseFromFile parseOp "AOC1.input"
  case ops of
    Right o -> error "no solution yet"
    Left e  -> error $ show e

solution2 :: IO String
solution2 = do
  ops <- parseFromFile parseOp "AOC1.input"
  case ops of
    Right o -> error "no solution yet"
    Left e  -> error $ show e
