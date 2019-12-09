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
       

data Layer = Layer {pixels::[Int], width :: Int, height :: Int} deriving (Show, Eq)

instance Ord Layer where
    compare l1 l2 = let zeros l = length $ filter (== 0) (pixels l) in
        zeros l1 `compare` zeros l2

instance Semigroup Layer where
    l1 <> l2 = let
        overlay 0 _ = 0
        overlay 1 _ = 1
        overlay _ x = x
        combined = zipWith overlay (pixels l1) (pixels l2)
        in Layer combined (width l1) (height l1)

instance Monoid Layer where
    mempty = Layer [2..] 0 0
 
pixelParser :: Parser Int
pixelParser = string "0" *> return 0 <|> string "1" *> return 1 <|> string "2" *> return 2

layerParser :: Int -> Int -> Parser Layer
layerParser w h = do pixels <- count (w * h) pixelParser
                     return $ Layer pixels w h

checksum :: Layer -> Int
checksum l = let ones = length $ filter (==1) (pixels l)
                 twos = length $ filter (==2) (pixels l)
             in ones * twos

printLayer :: Layer -> IO ()
printLayer l = do
    let pixels' = pixels l
        width'  = width l
    let fakeColor :: Int -> String
        fakeColor 0 = " "
        fakeColor 1 = "X"
        fakeColor 2 = " "
    let printLine [] = return ()
        printLine ps = do 
            putStrLn $ unwords $ fakeColor <$> take width' ps
            printLine (drop width' ps)
    printLine pixels'

-- 1572
solution1 :: IO Int
solution1 = do
    layers <- parseFromFile (many1 $ layerParser 25 6) "AOC8.input"
    case layers of
        Right l -> do 
            return . checksum . head . sort $ l
-- KYHFE
solution2 :: IO ()
solution2 = do 
    layers <- parseFromFile (many1 $ layerParser 25 6) "AOC8.input"
    case layers of
        Right l -> do 
            printLayer . mconcat $ l

