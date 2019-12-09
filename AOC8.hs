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
                                                , count
                                                )
import           Text.Parsec.ByteString         ( Parser
                                                , parseFromFile
                                                )
import Data.List (sort)
       

data Layer = Layer {pixels::[Int]} deriving (Show, Eq)

instance Ord Layer where
    compare l1 l2 = let zeros l = length $ filter (== 0) (pixels l) in
        zeros l1 `compare` zeros l2

pixelParser :: Parser Int
pixelParser = string "0" *> return 0 <|> string "1" *> return 1 <|> string "2" *> return 2

layerParser :: Int -> Int -> Parser Layer
layerParser w h = Layer <$> count (w * h) pixelParser

checksum :: Layer -> Int
checksum l = let ones = length $ filter (==1) (pixels l)
                 twos = length $ filter (==2) (pixels l)
             in ones * twos

-- 1572
solution1 :: IO Int
solution1 = do
    layers <- parseFromFile (many1 $ layerParser 25 6) "AOC8.input"
    case layers of
        Right l -> do 
            return . checksum . head . sort $ l

solution2 :: IO Int
solution2 = error "no solution yet"

