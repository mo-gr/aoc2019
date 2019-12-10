module AOC10 where

import           Text.Parsec                    ( digit
                                                , many1
                                                , many
                                                , parse
                                                , skipMany1
                                                , skipMany
                                                , space
                                                , string
                                                , (<|>)
                                                , sepBy
                                                , getPosition
                                                , sourceColumn
                                                , sourceLine
                                                )
import           Text.Parsec.ByteString         ( Parser
                                                , parseFromFile
                                                )
import qualified Data.Set as Set                                                

data Point = Point {_x :: Int, _y :: Int} deriving (Show, Eq, Ord)
type Angle = Double

asteroidParser :: Parser [Point]
asteroidParser = do
    _ <- string "#"
    pos <- getPosition
    return [Point {_x = sourceColumn pos, _y = sourceLine pos}]

emptyParser :: Parser [Point]    
emptyParser = string "." *> return []

lineParser :: Parser [Point]
lineParser = concat <$> (many1 (asteroidParser <|> emptyParser)) <* skipMany space

calculateAngle :: Point -> Point -> Angle
calculateAngle p1 p2 = atan2 (fromIntegral $ _x p2 - _x p1) (fromIntegral $ _y p2 - _y p1)

sightLines :: [Point] -> Point -> [(Angle, Point)]
sightLines [] _ = []
sightLines (p:ps) ref | ref == p = sightLines ps ref
sightLines (p:ps) ref = let angle = calculateAngle ref p
                        in (angle, p) : sightLines ps ref

countVisibles :: [(Angle, Point)] -> Int
countVisibles ps = let pSet = Set.fromList (fst <$> ps) in
    length pSet

-- 276
solution1 :: IO Int
solution1 = do
    pointsOrErrors <- parseFromFile (concat <$> many lineParser) "AOC10.input"
    case pointsOrErrors of
        Right points -> do
            return . maximum $ (countVisibles . sightLines points) <$> points
        Left e -> error . show $ e

solution2 :: IO ()
solution2 = error "no solution yet"
