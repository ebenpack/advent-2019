{-# LANGUAGE MultiWayIf #-}

module Drone where

import IntCode
import Util

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad

droneProgram = mkProgram [109,424,203,1,21101,11,0,0,1106,0,282,21102,1,18,0,1105,1,259,1202,1,1,221,203,1,21101,0,31,0,1105,1,282,21102,1,38,0,1106,0,259,21001,23,0,2,22102,1,1,3,21102,1,1,1,21102,57,1,0,1106,0,303,2101,0,1,222,21002,221,1,3,21001,221,0,2,21101,0,259,1,21101,80,0,0,1105,1,225,21101,158,0,2,21101,0,91,0,1106,0,303,1201,1,0,223,20102,1,222,4,21101,259,0,3,21101,225,0,2,21102,225,1,1,21101,118,0,0,1106,0,225,20102,1,222,3,21101,0,79,2,21102,1,133,0,1106,0,303,21202,1,-1,1,22001,223,1,1,21101,148,0,0,1105,1,259,2102,1,1,223,21001,221,0,4,20102,1,222,3,21101,16,0,2,1001,132,-2,224,1002,224,2,224,1001,224,3,224,1002,132,-1,132,1,224,132,224,21001,224,1,1,21101,0,195,0,106,0,108,20207,1,223,2,20101,0,23,1,21102,-1,1,3,21102,214,1,0,1106,0,303,22101,1,1,1,204,1,99,0,0,0,0,109,5,1201,-4,0,249,21202,-3,1,1,21201,-2,0,2,22101,0,-1,3,21101,250,0,0,1106,0,225,21202,1,1,-4,109,-5,2105,1,0,109,3,22107,0,-2,-1,21202,-1,2,-1,21201,-1,-1,-1,22202,-1,-2,-2,109,-3,2106,0,0,109,3,21207,-2,0,-1,1206,-1,294,104,0,99,21202,-2,1,-2,109,-3,2106,0,0,109,5,22207,-3,-4,-1,1206,-1,346,22201,-4,-3,-4,21202,-3,-1,-1,22201,-4,-1,2,21202,2,-1,-1,22201,-4,-1,1,22101,0,-2,3,21102,343,1,0,1106,0,303,1106,0,415,22207,-2,-3,-1,1206,-1,387,22201,-3,-2,-3,21202,-2,-1,-1,22201,-3,-1,3,21202,3,-1,-1,22201,-3,-1,2,22101,0,-4,1,21101,384,0,0,1105,1,303,1105,1,415,21202,-4,-1,-4,22201,-4,-3,-4,22202,-3,-2,-2,22202,-2,-4,-4,22202,-3,-2,-3,21202,-4,-1,-2,22201,-3,-2,1,21202,1,1,-4,109,-5,2106,0,0]

beamStart droneProgram = go 0
    where
    go x y =
        let p1@Program{ outputs_ = [out] } = runProgram droneProgram { inputs_ = [x, y] }
        in
            if out == 1
            then (x,y)
            else go (x + 1) y

beamEnd droneProgram = go
    where
    go x y =
        let p1@Program{ outputs_ = [out1] } = runProgram droneProgram { inputs_ = [x, y] }
            p2@Program{ outputs_ = [out2] } = runProgram droneProgram { inputs_ = [x + 1, y] }
        in
            if out1 == 1 && out2 == 0
            then (x,y)
            else go (x + 1) y

beamWidth droneProgram y = go startx y 0
    where
    (startx, starty) = beamStart droneProgram y
    go x y width =
        let p1@Program{ outputs_ = [out1] } = runProgram droneProgram { inputs_ = [x, y] }
            p2@Program{ outputs_ = [out2] } = runProgram droneProgram { inputs_ = [x + 1, y] }
            newWidth = width + if out1 == 1 then 1 else 0
        in
            if out1 == 1 && out2 == 0
            then newWidth
            else go (x + 1) y newWidth

--beamHeight droneProgram y = go startx starty 0
--   where
--   (startx, starty) = beamEnd droneProgram y
--   go x y height =
--       let p1@Program{ outputs_ = [out1] } = runProgram droneProgram { inputs_ = [x, y] }
--           p2@Program{ outputs_ = [out2] } = runProgram droneProgram { inputs_ = [x, y + 1] }
--           newheight = height + if out1 == 1 then 1 else 0
--       in
--           if out1 == 1 && out2 == 0
--           then newheight
--           else go x (y + 1) newheight

runDroneProgram droneProgram width height = go 0 0 Map.empty 0
    where
    go x y acc px
        | y >= height && x >= width = acc
        | x >= width = go px (y + 1) acc px
        | otherwise =
            let p1@Program{ outputs_ = [out1] } = runProgram droneProgram { inputs_ = [x, y] }
                p2@Program{ outputs_ = [out2] } = runProgram droneProgram { inputs_ = [x + 1, y] }
                px' = if out1 == 0 && out2 == 1 then x + 1 else px
            in
                if out1 == 1 && out2 == 0
                then go px' (y + 1) (Map.insert (x,y) 1 acc) px'
                else go (x + 1) y (Map.insert (x,y) out1 acc) px'

part1DroneAnswer = -- 126
    let result = runDroneProgram droneProgram 50 50
    in Map.size $ Map.filter (== 1) result

findFirstStableRowOfWidth droneProgram target y = go 0 y
    where
    newT = target - 1
    check x y =
        if x - newT <= 0 || y - newT <= 0
        then False
        else
            let Program{ outputs_ = [out1] } = runProgram droneProgram { inputs_ = [x - newT, y] }
                Program{ outputs_ = [out2] } = runProgram droneProgram { inputs_ = [x, y + newT] }
                Program{ outputs_ = [out3] } = runProgram droneProgram { inputs_ = [x - newT, y + newT] }
            in out1 == 1 && out2 == 1 && out3 == 1
    go x y =
        let (endx, _) = beamEnd droneProgram x y
        in
            if check endx y
            then (endx - newT, y)
            else go endx (y + 1)

part2DroneAnswer =
    let (x,y) = findFirstStableRowOfWidth droneProgram 100 1000
    in (x * 10000) + y

drawDroneMap droneProgram width height = draw
    where
    result = runDroneProgram droneProgram width height
    draw = do
        forM_ [ (x,y) | y <- [0..droneWidth], x <- [0..droneHeight]]
            (\(x,y) -> (if x == 0 then putStr "\n" else pure ()) >> printTile (x,y))
        putStr "\n\n"
    droneWidth = Set.findMax $ Set.map fst $ Map.keysSet result
    droneHeight = Set.findMax $ Set.map snd $ Map.keysSet result
    printTile (x,y) =
        case Map.lookup (x,y) result of
            Just 0 -> putStr "."
            Just 1 -> putStr "#"
            Nothing -> putStr "."