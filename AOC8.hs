{-# LANGUAGE OverloadedStrings          #-}

module AOC8 where

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
       

solution1 :: IO Int
solution1 = error "no solution yet"

solution2 :: IO Int
solution2 = error "no solution yet"

