{-# LANGUAGE MultiWayIf #-}

module RepairDroid where

import IntCode
import Util

import Data.List
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Tile = Floor | Wall | Oxy deriving (Eq, Show)

type FloorMap = Map.Map Point Tile

data Direction = N | S | E | W deriving (Eq, Show, Bounded, Enum)

data DroidStatus = HitWall | MovedOneStep | AtOxy deriving (Eq, Show)

newtype Droid = Droid { location_ :: Point } deriving (Show)

newDroid :: Droid
newDroid = Droid { location_ = (0, 0) }

intToDroidStatus :: Integer -> DroidStatus
intToDroidStatus 0 = HitWall
intToDroidStatus 1 = MovedOneStep
intToDroidStatus 2 = AtOxy

intToDirection :: Integer -> Direction
intToDirection 1 = N
intToDirection 2 = S
intToDirection 3 = W
intToDirection 4 = E

directionToInt :: Direction -> Integer
directionToInt N = 1
directionToInt S = 2
directionToInt W = 3
directionToInt E = 4

moveInDirection :: Point -> Direction -> Point
moveInDirection (x, y) N = (x    , y + 1)
moveInDirection (x, y) S = (x    , y - 1)
moveInDirection (x, y) E = (x + 1, y    )
moveInDirection (x, y) W = (x - 1, y    )

droidStatusToTile :: DroidStatus -> Tile
droidStatusToTile HitWall = Wall
droidStatusToTile MovedOneStep = Floor
droidStatusToTile AtOxy = Oxy

droidProg :: Program
droidProg = mkProgram [3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,101,0,1034,1039,101,0,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1106,0,124,1002,1034,1,1039,1001,1036,0,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,1002,1035,1,1040,1001,1038,0,1043,101,0,1037,1042,1105,1,124,1001,1034,1,1039,1008,1036,0,1041,101,0,1035,1040,102,1,1038,1043,1001,1037,0,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,1,1032,1006,1032,165,1008,1040,7,1032,1006,1032,165,1101,2,0,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1102,1,1,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,45,1044,1106,0,224,1102,1,0,1044,1105,1,224,1006,1044,247,101,0,1039,1034,1001,1040,0,1035,102,1,1041,1036,1002,1043,1,1038,1001,1042,0,1037,4,1044,1105,1,0,20,12,24,92,28,41,2,48,89,3,20,28,54,25,52,5,1,6,33,88,74,9,9,37,88,28,76,41,47,37,36,57,47,29,66,5,85,31,41,36,91,73,35,57,47,84,35,24,73,58,46,6,12,33,71,36,91,84,10,11,63,23,54,49,36,43,17,37,67,92,8,90,27,35,73,21,43,93,43,23,73,13,21,92,17,93,9,82,29,43,75,91,64,28,78,83,6,5,87,81,44,44,25,64,36,90,89,39,50,1,99,8,46,61,82,44,30,92,83,27,9,58,96,4,48,22,98,22,14,58,11,36,98,14,71,29,63,95,23,70,74,20,97,35,96,18,29,68,20,69,39,56,2,37,82,15,34,29,88,86,11,13,75,1,73,48,59,71,44,42,83,89,17,53,82,1,70,35,79,28,82,62,2,62,8,79,11,20,27,50,6,77,47,27,4,24,64,37,22,84,27,49,40,13,98,25,28,98,94,18,10,40,95,6,27,11,54,43,30,53,5,72,73,92,44,30,61,9,97,84,18,30,65,17,34,75,86,47,1,32,14,70,32,27,84,65,63,37,57,90,25,64,7,54,76,29,94,33,53,29,58,21,3,81,88,50,16,53,24,28,96,64,12,36,67,13,33,67,78,43,90,20,46,31,44,87,30,35,85,94,22,86,12,63,92,6,43,24,47,26,64,77,39,21,76,9,63,79,17,34,61,4,1,19,63,89,30,85,19,95,58,91,16,97,35,50,81,3,59,37,96,17,79,12,46,81,9,64,47,10,48,25,64,2,62,69,23,32,71,77,41,28,65,98,7,39,76,31,61,41,18,56,39,80,95,24,41,38,97,29,32,65,42,97,10,91,68,5,27,55,35,94,4,10,69,22,40,2,81,5,88,1,99,3,99,75,7,87,60,39,26,53,14,20,80,94,2,49,19,79,41,46,42,9,82,13,51,76,19,75,18,28,89,56,21,92,86,17,58,17,30,50,19,34,47,71,1,93,21,36,90,77,40,20,80,63,17,7,52,79,10,53,64,24,40,4,64,24,39,77,55,31,29,91,77,46,36,15,44,96,22,19,98,76,2,20,9,99,76,2,87,84,31,47,3,16,95,84,4,32,13,56,34,79,93,18,89,92,12,92,80,33,78,42,76,33,14,42,64,81,5,54,15,92,97,56,29,5,63,21,6,76,58,65,28,29,58,18,73,49,25,95,59,40,59,5,15,72,36,62,43,77,1,20,48,31,90,22,63,21,79,31,75,24,21,64,84,16,65,91,38,35,29,57,72,61,73,5,35,94,36,16,66,17,88,56,6,41,75,6,25,87,27,68,42,23,66,19,21,76,2,33,92,21,76,44,30,79,42,46,63,59,41,94,20,66,3,71,60,54,82,2,17,98,38,90,95,3,15,65,53,39,92,6,20,62,53,33,12,52,92,39,60,72,41,86,16,40,25,63,14,21,32,24,10,68,97,38,33,40,97,93,43,40,1,94,84,27,23,71,9,68,32,19,15,25,71,57,10,52,25,92,12,72,90,42,97,79,4,73,83,25,80,26,68,35,8,91,47,43,15,57,76,68,37,29,92,92,24,52,53,37,26,94,23,49,18,20,63,38,5,15,77,66,39,89,14,20,19,80,15,63,81,3,60,74,13,33,85,71,94,7,18,95,75,34,73,23,28,99,35,77,60,71,37,74,43,50,46,55,28,97,16,90,21,60,89,88,52,48,39,72,3,46,43,77,17,79,20,71,41,67,26,99,13,54,90,64,20,75,0,0,21,21,1,10,1,0,0,0,0,0,0]

mapSpace :: Program -> FloorMap
mapSpace = go Map.empty newDroid
    where
    updateDroid :: FloorMap -> Droid -> Point -> DroidStatus -> (Droid, FloorMap)
    updateDroid currentMap d newLoc stat =
        case stat of
            HitWall -> (d, Map.insert newLoc Wall currentMap)
            MovedOneStep -> (d { location_ = newLoc }, Map.insert newLoc Floor currentMap)
            AtOxy -> (d { location_ = newLoc }, Map.insert newLoc Oxy currentMap)
    makeAMove :: Droid -> Program -> FloorMap -> Direction -> FloorMap
    makeAMove droid@Droid{ location_ = currentLocation } prog currentMap move =
        let newLocation = moveInDirection currentLocation move
        in
            if Map.member newLocation currentMap
            then currentMap
            else
                let newProg = runProgram (prog { inputs_ = [ directionToInt move ], status_ = Running })
                    droidOutput = intToDroidStatus $ head $ outputs_ newProg -- YOLO!
                    (newDroid, newMap) = updateDroid currentMap droid newLocation droidOutput
                in go newMap newDroid (newProg { outputs_ = [] })
    go :: FloorMap -> Droid -> Program -> FloorMap
    go floorMap d prog =
        let newMoves = intToDirection <$> [1..4]
        in foldl' (makeAMove d prog) floorMap newMoves

findNeighbors :: Point -> FloorMap -> [Point]
findNeighbors location floorMap =
    let newMoves = moveInDirection location . intToDirection <$> [1..4]
    in [ move | move <- newMoves, Map.member move floorMap, Map.lookup move floorMap /= Just Wall ]

floorMapToTree :: FloorMap -> Point -> Tree Point
floorMapToTree floorMap startingPoint = go startingPoint Set.empty
    where
    go currentNode discovered =
        if Set.member currentNode discovered
        then Empty
        else
            let neighbors = filter (\neigh -> not $ Set.member neigh discovered) $ findNeighbors currentNode floorMap
                newlyDiscovered = Set.insert currentNode discovered
                children = map (`go` newlyDiscovered) neighbors
            in Tree { node_ = currentNode, children_ = children }

runDroidProg :: Program -> (Int, Int)
runDroidProg prog =
    let floorMap = mapSpace prog
        startingPoint = (0, 0)
        destination = fst $ head $ Map.toList (Map.filter (== Oxy) floorMap) -- Double YOLO!
        floorTreeFromOrigin = floorMapToTree floorMap startingPoint
        shortestPathLen = findShortestPathLength floorTreeFromOrigin destination
        floorTreeFromOyx = floorMapToTree floorMap destination
        longestPathLen = findLongestPathLength floorTreeFromOyx
    in (shortestPathLen, longestPathLen)
