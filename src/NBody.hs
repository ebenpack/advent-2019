module NBody where

import Data.List
import qualified Data.Set as Set
import Control.Monad (guard)

type Position = (Int,Int,Int)

type Velocity = (Int,Int,Int)

data Moon = Moon { position_ :: Position, velocity_ :: Velocity } deriving (Eq, Ord)

instance Show Moon where
    show Moon { position_ = (x,y,z), velocity_ = (vx,vy,vz) } =
        "pos=<x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z ++ ">, " ++
        "vel=<x=" ++ show vx ++ ", y=" ++ show vy ++ ", z=" ++ show vz ++ ">"

makeMoons :: [Position] -> [Moon]
makeMoons = map (\p -> Moon { position_ = p, velocity_ = (0,0,0) })

testmoons :: [Moon]
testmoons = makeMoons [(-1, 0, 2),(2,  -10, -7),(4,  -8,  8),(3,  5,   -1)]

moons :: [Moon]
moons = makeMoons [(13, 9, 5),(8, 14,  -2),(-5, 4, 11),(2, -6, 1)]

applyGravity :: [Moon] -> [Moon]
applyGravity moons = map applyG moons
    where
    applyG m = foldl' calculateNewVelocity m moons
    calculateNewVelocity m1 m2 =
        let (x1, y1, z1) = position_ m1
            (vx1, vy1, vz1) = velocity_ m1
            (x2, y2, z2) = position_ m2
            (vx2, vy2, vz2) = velocity_ m2
            newVelocity =
                ( vx1 + velocityDelta x1 x2
                , vy1 + velocityDelta y1 y2
                , vz1 + velocityDelta z1 z2
                )
        in m1 { velocity_ = newVelocity }
    velocityDelta a b
        | a < b = 1
        | a > b = -1
        | otherwise = 0

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map addVelocityToPosition
    where
    addVelocityToPosition m@Moon { position_ = (x,y,z), velocity_ = (vx,vy,vz) } =
        let newPosition = (x + vx, y + vy, z + vz)
        in m { position_ = newPosition }

calculateTotalSystemEnergy :: [Moon] -> Int
calculateTotalSystemEnergy = sum . map (\m -> potentialEnergy m * kineticEnergy m)
    where
    potentialEnergy Moon{ position_ = (x,y,z) } = abs x + abs y + abs z
    kineticEnergy Moon{ velocity_ = (vx,vy,vz) } = abs vx + abs vy + abs vz

runSimulationStep :: [Moon] -> [Moon]
runSimulationStep = applyVelocity . applyGravity

runNSimulationSteps :: Int -> [Moon] -> [Moon]
runNSimulationSteps 0 m = m
runNSimulationSteps n m = runNSimulationSteps (n - 1) (runSimulationStep m)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

trd3 :: (a,b,c) -> c
trd3 (_,_,c) = c

findDimensionalPeriod :: (Moon -> (Int, Int)) -> [Moon] -> Integer
findDimensionalPeriod dimensionalExtractor initialMoons =
    let initialDimensionalState = map dimensionalExtractor initialMoons
    in go initialDimensionalState initialMoons 0
    where
    go initialDimensionalState moons steps =
        let nextState = runSimulationStep moons
            nextSteps = steps + 1
        in
            if map dimensionalExtractor nextState == initialDimensionalState
            then nextSteps
            else go initialDimensionalState nextState nextSteps

extractDimension :: ((Int,Int,Int) -> Int) -> Moon -> (Int, Int)
extractDimension extract Moon { position_ = pos, velocity_ = vel } = (extract pos, extract vel)

extractXDimension :: Moon -> (Int, Int)
extractXDimension = extractDimension fst3

extractYDimension :: Moon -> (Int, Int)
extractYDimension = extractDimension snd3

extractZDimension:: Moon -> (Int, Int)
extractZDimension = extractDimension trd3

runSimulationUntilRepetition :: [Moon] -> Integer
runSimulationUntilRepetition moons =
    let xPeriod = findDimensionalPeriod extractXDimension moons
        yPeriod = findDimensionalPeriod extractYDimension moons
        zPeriod = findDimensionalPeriod extractZDimension moons
    in lcm (lcm xPeriod yPeriod) zPeriod