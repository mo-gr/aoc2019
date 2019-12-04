{-# LANGUAGE OverloadedStrings          #-}

module AOC3 where

import           Text.Parsec            (digit, many1, parse, skipMany, space,
                                         string, (<|>), sepBy1)
import           Text.Parsec.ByteString (Parser, parseFromFile)
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set

data Point = Point {_x :: Int, _y :: Int} deriving (Show, Eq, Ord)

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)

data Line = Line {_direction :: Direction, _length :: Int} deriving (Show, Eq)

type Wire = [Line]

number :: Parser Int
number = read <$> many1 digit

directionP :: Parser Direction
directionP = string "R" $> RIGHT
  <|> string "L" $> LEFT
  <|> string "U" $> UP
  <|> string "D" $> DOWN

lineP :: Parser Line
lineP = do
  dir <- directionP
  len <- number
  return $ Line {_direction = dir, _length = len}

wireP :: Parser Wire
wireP = lineP `sepBy1` string ","

parseOp = many1 (wireP <* skipMany space)

--

lineToPoints :: [Point] -> Line -> [Point]
lineToPoints [] l = lineToPoints [Point 0 0] l
lineToPoints ps Line {_length = 0} = ps
lineToPoints ps@(p:_) l@Line { _direction = dir} = lineToPoints ((nextPoint p dir):ps) (shorten l)

nextPoint :: Point -> Direction -> Point
nextPoint Point {_x=x, _y=y} UP = Point {_x=x, _y=y+1}
nextPoint Point {_x=x, _y=y} DOWN = Point {_x=x, _y=y-1}
nextPoint Point {_x=x, _y=y} LEFT = Point {_x=x-1, _y=y}
nextPoint Point {_x=x, _y=y} RIGHT = Point {_x=x+1, _y=y}

shorten :: Line -> Line
shorten Line {_direction = d, _length = l} = Line {_direction=d, _length=(l - 1)}

wireToPointCloud :: Wire -> Set Point
wireToPointCloud lines = Set.fromList . mconcat $ (lineToPoints []) <$> lines

-- example 159

solution1 :: IO (Int, Int)
solution1 = do
  ops <- parseFromFile parseOp "example.txt"
  case ops of
    Right wires -> print $ wireToPointCloud <$> wires
  error "no solution yet"

solution2 :: IO ()
solution2 = error "no solution yet"
