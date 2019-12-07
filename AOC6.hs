{-# LANGUAGE OverloadedStrings          #-}
module AOC6 where

import           Text.Parsec                    ( digit
                                                , many1
                                                , parse
                                                , skipMany
                                                , alphaNum
                                                , space
                                                , string
                                                , (<|>)
                                                , sepBy
                                                )
import           Text.Parsec.ByteString         ( Parser
                                                , parseFromFile
                                                )
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (fromMaybe)

data Planet = Planet { name :: String } deriving (Eq, Show)
data Orbit = Orbit { center :: Planet, outer :: Planet } deriving (Eq, Show)
type WeightedOrbit = (Orbit, Int)

com :: Planet
com = Planet "COM"

comParser :: Parser Planet
comParser = string "COM" $> com

planetParser :: Parser Planet
planetParser = do
    name <- many1 alphaNum
    return $ case name of
        "COM" -> com
        _ -> Planet name

orbitParser :: Parser Orbit
orbitParser = do
    center' <- planetParser
    _ <- string ")"
    Orbit center' <$> planetParser

inputParser :: Parser [Orbit]
inputParser = many1 (orbitParser <* skipMany space)

parentOrbit :: [Orbit] -> Planet -> Maybe Orbit
parentOrbit _ p | p == com = Nothing
parentOrbit os p = find (\o' -> outer o' == p) os

calculateDistanceToCom :: [Orbit] -> Orbit -> WeightedOrbit
calculateDistanceToCom _ o | center o == com = (o,1)
calculateDistanceToCom os o = fromMaybe (o,1) $ do
    let c = center o
    po <- parentOrbit os c
    let pw = snd $ calculateDistanceToCom os po
    return (o, 1 + pw)

orbitSum :: [WeightedOrbit] -> Int
orbitSum = sum . map snd

-- 194721
solution1 :: IO Int
solution1 = do
    orbs <- parseFromFile inputParser "AOC6.input"
    case orbs of
        Right orbs' -> return $ orbitSum $ map (calculateDistanceToCom orbs') orbs'
        Left e -> error (show e)

solution2 :: IO Int
solution2 = error "no solution yet"

-- TESTS

prop_parser :: H.Property
prop_parser =
    H.withTests 1 $ H.property $
      case parse inputParser "test" "COM)A" of
        Right x -> x H.=== [Orbit com (Planet "A")]
        Left e -> H.footnote (show e) >> H.failure 

prop_example :: H.Property
prop_example =
    H.withTests 1 $ H.property $
      case parse inputParser "example" example of
        Right x -> (snd <$> map (calculateDistanceToCom x) x) H.=== [1 , 2 , 3 , 4 , 5 , 2 , 3 , 4 , 5 , 6 , 7]
        Left e -> H.footnote (show e) >> H.failure 

example = "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L" 

tests :: IO Bool
tests = H.checkParallel $ H.Group "AOC5" [
        ("prop_parser", prop_parser),
        ("prop_example", prop_example)
    ]
        