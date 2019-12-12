{-# LANGUAGE OverloadedStrings, NamedFieldPuns          #-}
module AOC12
    ( solution1
    , solution2
    )
where

import           Control.Monad                  ( liftM2 )
type Value = Int
data Vec3 = Vec3 {_x :: Value, _y :: Value, _z :: Value} deriving (Show, Eq, Ord)

newtype Velocity = Velocity Vec3 deriving (Show, Eq, Ord)
newtype Position = Position Vec3 deriving (Show, Eq, Ord)

data Moon = Moon {name::String, position::Position, velocity::Velocity} deriving (Show, Eq)

makeMoon :: String -> Vec3 -> Moon
makeMoon name' p = Moon name' (Position p) (Velocity $ Vec3 0 0 0)

io :: Moon
io = makeMoon "io" $ Vec3 9 13 (-8)
europa :: Moon
europa = makeMoon "europa" $ Vec3 (-3) 16 (-17)
ganymede :: Moon
ganymede = makeMoon "ganymede" $ Vec3 (-4) 11 (-10)
callisto :: Moon
callisto = makeMoon "callisto" $ Vec3 0 (-2) (-2)

universe :: [Moon]
universe = [io, europa, ganymede, callisto]

kineticEnergy :: Moon -> Value
kineticEnergy Moon { velocity = Velocity Vec3 { _x, _y, _z } } =
    abs _x + abs _y + abs _z

potentialEnergy :: Moon -> Value
potentialEnergy Moon { position = Position Vec3 { _x, _y, _z } } =
    abs _x + abs _y + abs _z

totalEnergy :: Moon -> Value
totalEnergy = liftM2 (+) kineticEnergy potentialEnergy

totalEnergySystem :: [Moon] -> Value
totalEnergySystem = sum . fmap totalEnergy 

gravityPull :: Moon -> Moon -> Moon
gravityPull m@Moon { position = Position pVec, velocity} Moon { position = Position pVec' }= 
    m {velocity=velPlus velocity gravity} where
        gravity = Velocity (delta pVec pVec')

velPlus :: Velocity -> Velocity -> Velocity
velPlus (Velocity pVec) (Velocity pVec') = Velocity (pVec `plus` pVec')

plus :: Vec3 -> Vec3 -> Vec3
plus Vec3{_x, _y, _z} Vec3{_x=_x', _y=_y', _z=_z'} = Vec3 (_x + _x') (_y + _y') (_z + _z')

delta :: Vec3 -> Vec3 -> Vec3
delta Vec3{_x, _y, _z} Vec3{_x=_x', _y=_y', _z=_z'} = Vec3 (f _x _x') (f _y _y') (f _z _z')
  where f x x' | x < x' = (-1)
        f x x' | x > x' = 1
        f _ _ = 0

applyVelocity :: Moon -> Moon
applyVelocity m@Moon {velocity = Velocity vVec, position = Position pVec } = m {position=Position (vVec `plus` pVec)}

applyGravity :: [Moon] -> Moon -> Moon
applyGravity [] m = m
applyGravity (other:ms) m = m `gravityPull` other `gravityPull` applyGravity ms m

tick :: [Moon] -> [Moon]
tick ms = applyVelocity . applyGravity ms <$> ms

times :: Int -> (a -> a) -> a -> a
times 0 _ a = a
times x f a = times (x-1) f (f a)

-- too high: 4513569
solution1 :: IO Int
solution1 = return . totalEnergySystem $ times 1000 tick universe

solution2 :: IO ()
solution2 = error "no solution"
