{-# LANGUAGE RecordWildCards #-}

module PaintRobot where

import IntCode
import Util

import qualified Data.Map as Map
import Data.Maybe
import Codec.Picture (PixelRGBA8(..), generateImage)
import Codec.Picture.Bitmap (writeBitmap)

data Direction = L | R | U | D deriving (Show, Eq)

data Turn = TurnLeft | TurnRight deriving (Show, Eq)

data Color = B | W deriving (Show, Eq)

type Hull = Map.Map Point Color

data Robot = Robot { location_ :: Point, direction_ :: Direction } deriving (Show)

xCoord :: Point -> Int
xCoord (x,_) = x

yCoord :: Point -> Int
yCoord (_,y) = y

newRobot :: Robot
newRobot = Robot { location_ = (0,0), direction_ = U }

newHull :: Hull
newHull = Map.empty

intToDir :: Integer -> Turn
intToDir 0 = TurnLeft
intToDir 1 = TurnRight

turn :: Direction -> Turn -> Direction
turn L TurnLeft = D
turn R TurnLeft = U
turn U TurnLeft = L
turn D TurnLeft = R
turn L TurnRight = U
turn R TurnRight = D
turn U TurnRight = R
turn D TurnRight = L

intToCol :: Integer -> Color
intToCol 0 = B
intToCol 1 = W

colToInt :: Color -> Integer
colToInt B = 0
colToInt W = 1

paintProg = mkProgram [3,8,1005,8,325,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,28,2,3,7,10,2,1109,3,10,2,102,0,10,2,1005,12,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,67,2,109,12,10,1,1003,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,96,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,119,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,141,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,162,1,106,17,10,1006,0,52,1006,0,73,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,194,1006,0,97,1,1004,6,10,1006,0,32,2,8,20,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,102,1,8,231,1,1,15,10,1006,0,21,1,6,17,10,2,1005,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,267,2,1007,10,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,294,1006,0,74,2,1003,2,10,1,107,1,10,101,1,9,9,1007,9,1042,10,1005,10,15,99,109,647,104,0,104,1,21101,936333018008,0,1,21101,342,0,0,1106,0,446,21102,937121129228,1,1,21101,0,353,0,1105,1,446,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,209383001255,1,21102,400,1,0,1106,0,446,21101,0,28994371675,1,21101,411,0,0,1105,1,446,3,10,104,0,104,0,3,10,104,0,104,0,21101,867961824000,0,1,21101,0,434,0,1106,0,446,21102,1,983925674344,1,21101,0,445,0,1106,0,446,99,109,2,21201,-1,0,1,21102,40,1,2,21101,477,0,3,21102,467,1,0,1106,0,510,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,472,473,488,4,0,1001,472,1,472,108,4,472,10,1006,10,504,1101,0,0,472,109,-2,2106,0,0,0,109,4,1201,-1,0,509,1207,-3,0,10,1006,10,527,21102,1,0,-3,21202,-3,1,1,21201,-2,0,2,21102,1,1,3,21102,1,546,0,1106,0,551,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,574,2207,-4,-2,10,1006,10,574,22101,0,-4,-4,1105,1,642,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21101,0,593,0,1105,1,551,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,612,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,634,21201,-1,0,1,21101,634,0,0,105,1,509,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0]

colorAtPoint :: Point -> Hull -> Color
colorAtPoint = Map.findWithDefault B

paint :: Point -> Color -> Hull -> Hull
paint = Map.insert

move :: Direction -> Point -> Point
move L (x,y) = (x - 1, y)
move R (x,y) = (x + 1, y)
move U (x,y) = (x, y - 1)
move D (x,y) = (x, y + 1)

runRobotProgram :: Program -> Robot -> Hull -> Hull
runRobotProgram prog robot hull =
    case runProgram prog of
        Program { status_ = Halted } -> hull
        p ->
            let [dirToTurn, colToPaint] = outputs_ p
                oldDirection = direction_ robot
                newDirection = turn oldDirection $ intToDir dirToTurn
                currentLocation = location_ robot
                newHull = paint currentLocation (intToCol colToPaint) hull
                newLocation = move newDirection currentLocation
                newRobot = robot { direction_ = newDirection, location_ = newLocation }
                newInp = [ colToInt $ colorAtPoint newLocation newHull ]
                newProg = p { inputs_ = newInp, outputs_ = [], status_ = Running }
            in runRobotProgram newProg newRobot newHull

hullToImg :: Hull -> IO ()
hullToImg hull = do
    let allPoints = Map.keys hull
        maxX = maximum . map xCoord $ allPoints
        minX = minimum . map xCoord $ allPoints
        maxY = maximum . map yCoord $ allPoints
        minY = minimum . map yCoord $ allPoints
        width = (maxX - minX) + 20 -- this padding shouldn't be necessary
        height = (maxY - minY) + 20
        image = generateImage writePixel width height
    writeBitmap "./hullimage.bmp" image
    where
    writePixel x y =
        let color = Map.findWithDefault B (x,y) hull
        in
            case color of
                B -> PixelRGBA8 0 0 0 255
                W -> PixelRGBA8 255 255 255 255

runRobot :: IO ()
runRobot = do
    hullToImg $ runRobotProgram (paintProg { inputs_ = [colToInt W] } ) newRobot newHull
    pure ()