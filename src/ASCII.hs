{-# LANGUAGE MultiWayIf #-}

module ASCII where

import IntCode
import Util

import Data.Vector ((//))
import Data.List
import Data.List.Utils
import qualified Data.Set as Set
import Data.Char (chr, ord)
import Control.Monad (guard)

data Direction = N | S | E | W deriving (Eq, Show)

data MovementInstruction = L | R | M Int deriving (Eq, Show)

type MovementStrategy = [MovementInstruction] -- lol

data VacuumBot = VacuumBot { location_ :: Point, direction_ :: Direction } deriving (Show)

charToDir :: Char -> Direction
charToDir '^' = N
charToDir 'v' = S
charToDir '<' = W
charToDir '>' = E

asciiProg :: Program
asciiProg = mkProgram [1,330,331,332,109,4718,1101,0,1182,16,1101,1469,0,24,101,0,0,570,1006,570,36,101,0,571,0,1001,570,-1,570,1001,24,1,24,1106,0,18,1008,571,0,571,1001,16,1,16,1008,16,1469,570,1006,570,14,21102,58,1,0,1106,0,786,1006,332,62,99,21101,333,0,1,21101,0,73,0,1105,1,579,1102,1,0,572,1101,0,0,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,101,0,574,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1105,1,81,21101,0,340,1,1106,0,177,21102,477,1,1,1105,1,177,21101,0,514,1,21102,176,1,0,1105,1,579,99,21102,184,1,0,1106,0,579,4,574,104,10,99,1007,573,22,570,1006,570,165,1001,572,0,1182,21102,375,1,1,21102,211,1,0,1106,0,579,21101,1182,11,1,21101,222,0,0,1106,0,979,21102,388,1,1,21101,233,0,0,1105,1,579,21101,1182,22,1,21101,244,0,0,1106,0,979,21101,0,401,1,21101,255,0,0,1106,0,579,21101,1182,33,1,21102,1,266,0,1105,1,979,21101,0,414,1,21101,0,277,0,1105,1,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21102,1,1182,1,21101,313,0,0,1106,0,622,1005,575,327,1101,1,0,575,21101,0,327,0,1105,1,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,28,8,0,109,4,2102,1,-3,586,21002,0,1,-1,22101,1,-3,-3,21101,0,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1105,1,597,109,-4,2105,1,0,109,5,1201,-4,0,630,20101,0,0,-2,22101,1,-4,-4,21101,0,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,653,20101,0,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21101,702,0,0,1105,1,786,21201,-1,-1,-1,1106,0,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21102,1,731,0,1106,0,786,1105,1,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,756,0,0,1105,1,786,1106,0,774,21202,-1,-11,1,22101,1182,1,1,21102,1,774,0,1105,1,622,21201,-3,1,-3,1105,1,640,109,-5,2106,0,0,109,7,1005,575,802,20101,0,576,-6,20102,1,577,-5,1106,0,814,21102,0,1,-1,21101,0,0,-5,21101,0,0,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,57,-3,22201,-6,-3,-3,22101,1469,-3,-3,1201,-3,0,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21101,0,1,-1,1106,0,924,1205,-2,873,21101,0,35,-4,1105,1,924,1202,-3,1,878,1008,0,1,570,1006,570,916,1001,374,1,374,2101,0,-3,895,1101,2,0,0,1201,-3,0,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,921,21001,0,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,57,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,57,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21101,973,0,0,1106,0,786,99,109,-7,2106,0,0,109,6,21102,0,1,-4,21101,0,0,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1105,1,1041,21102,1,-4,-2,1106,0,1041,21102,-5,1,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,1201,-2,0,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1105,1,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1105,1,989,21102,1,439,1,1105,1,1150,21101,477,0,1,1106,0,1150,21102,1,514,1,21102,1,1149,0,1106,0,579,99,21102,1,1157,0,1106,0,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1201,-5,0,1176,2102,1,-4,0,109,-6,2106,0,0,18,7,50,1,5,1,50,1,5,1,50,1,5,1,50,1,5,1,50,1,5,1,50,1,5,1,50,1,5,1,50,11,19,9,24,1,23,1,7,1,24,13,11,1,7,1,36,1,11,1,7,1,36,1,11,1,7,1,36,1,11,1,7,1,36,1,9,11,36,1,9,1,1,1,44,1,9,1,1,1,44,1,9,1,1,1,44,13,54,1,56,1,56,1,56,1,56,1,56,1,56,1,48,9,48,1,56,1,56,1,56,1,56,1,56,1,56,1,28,9,19,1,28,1,7,1,19,1,28,1,7,1,19,1,28,1,7,1,19,1,28,1,7,1,19,11,18,1,7,1,29,1,16,13,1,1,25,1,16,1,1,1,7,1,1,1,1,1,25,1,16,1,1,1,7,11,19,1,16,1,1,1,9,1,1,1,5,1,19,1,8,11,9,1,1,1,5,1,19,1,8,1,7,1,11,1,1,1,5,1,19,1,8,1,7,1,11,1,1,1,5,1,11,9,8,1,7,1,11,1,1,1,5,1,11,1,16,1,7,1,11,13,7,1,16,1,7,1,13,1,5,1,3,1,7,1,16,9,13,7,3,1,7,1,48,1,7,1,48,1,7,1,48,1,7,1,48,1,7,1,48,1,7,1,48,9,16]

interpretAsciiOutput :: [Integer] -> (Set.Set Point, VacuumBot)
interpretAsciiOutput xs =
    let charred = (lines $ map (chr . fromIntegral) xs)
        scaffolding = Set.fromList $ do (y, row) <- zip [0..] $ reverse charred
                                        (x, cell) <- zip [0..] $ reverse row
                                        guard $ cell == '#'
                                        pure (x,y)
        bot = do (y, row) <- zip [0..] $ reverse charred
                 (x, cell) <- zip [0..] $ reverse row
                 guard $ cell == '^' || cell == 'v' || cell == '<' || cell == '>'
                 pure $ VacuumBot { location_ = (x,y), direction_ = charToDir cell }
    in (scaffolding, head bot) -- YOLO

findNeighbors :: Set.Set Point -> Point -> Set.Set Point
findNeighbors scaffolding (x,y) =
    let possibleNeighbors = Set.fromList [(x+x', y+y') | x' <- [-1..1], y' <- [-1..1], abs x' /= abs y' ]
    in scaffolding `Set.intersection` possibleNeighbors

findNextTurn :: Set.Set Point -> Point -> Direction -> MovementInstruction
findNextTurn scaffolding p d =
    if | Set.member (move (turn L d) p) scaffolding -> L
       | Set.member (move (turn R d) p) scaffolding -> R
       | otherwise -> error "WTF! wrong turn!"

findIntersections :: Set.Set Point -> Set.Set Point
findIntersections scaffolding = Set.filter ((== 4) . Set.size . findNeighbors scaffolding) scaffolding

turn :: MovementInstruction -> Direction -> Direction
turn R N = E
turn R S = W
turn R E = S
turn R W = N
turn L N = W
turn L S = E
turn L E = N
turn L W = S

move :: Direction -> Point -> Point
move N (x,y) = (x     , y - 1)
move S (x,y) = (x     , y + 1)
move E (x,y) = (x + 1 , y    )
move W (x,y) = (x - 1 , y    )

findAPath :: Set.Set Point -> Set.Set Point -> VacuumBot -> [MovementInstruction]
findAPath scaffolding intersections = go Set.empty []
    where
    moveForward visited b@VacuumBot{ location_ = loc, direction_ = dir } steps =
        if not $ Set.member (move dir loc) scaffolding
        then (steps, Set.insert loc visited, b)
        else moveForward (Set.insert loc visited) b { location_ = move dir loc } (steps + 1)
    go :: Set.Set Point -> [MovementInstruction] -> VacuumBot -> [MovementInstruction]
    go visited path b@VacuumBot{ location_ = loc, direction_ = dir }
        | Set.null (Set.difference scaffolding visited) = reverse path
        | not $ Set.member (move dir loc) scaffolding =
            let nextTurn = findNextTurn scaffolding loc dir
            in go visited (nextTurn:path) b { direction_ = turn nextTurn dir }
        | otherwise =
            let (steps, newVisited, newBot) = moveForward visited b 0
            in go newVisited (M steps:path) newBot

movementToInt :: MovementInstruction -> [Integer]
movementToInt L = [fromIntegral $ ord 'L']
movementToInt R = [fromIntegral $ ord 'R']
movementToInt (M n) = fromIntegral . ord <$> intToString n

movementToString :: MovementInstruction -> String
movementToString L = "L"
movementToString R = "R"
movementToString (M n) = intToString n

-- wtf, i dont need all these shits
movementStrategyToIntCode :: MovementStrategy -> [Integer]
movementStrategyToIntCode = concatMap movementToInt

movementStrategyToStringList :: MovementStrategy -> [String]
movementStrategyToStringList = map movementToString

movementStrategyToString :: MovementStrategy -> String
movementStrategyToString = concat . movementStrategyToStringList

movementStrategyToStringWithCommas :: MovementStrategy -> String
movementStrategyToStringWithCommas = intercalate "," . movementStrategyToStringList

movementStrategyToIntCodeWithCommas :: MovementStrategy -> [Integer]
movementStrategyToIntCodeWithCommas ms = fromIntegral . ord <$> movementStrategyToStringWithCommas ms

moveFunctionLength xs = length $ intersperse 0 $ movementStrategyToIntCode xs

findSubsequence :: Int -> [MovementStrategy] -> MovementStrategy
findSubsequence maxLen = go [] []
    where
    tooLong :: MovementStrategy -> Bool
    tooLong xs = moveFunctionLength xs > maxLen
    -- whoo boy, this some ugly ass shit
    go :: MovementStrategy -> MovementStrategy -> [MovementStrategy] -> MovementStrategy
    go subseq bestsubseq [] = bestsubseq
    go subseq bestsubseq ([]:xs) = go [] (if length subseq > length bestsubseq then subseq else bestsubseq) xs
    go subseq bestsubseq ((x:xs):rest) =
        let possibleSubseq = (subseq++[x])
        in
            if any (isSubsequenceOf possibleSubseq) (xs:rest) && not (tooLong possibleSubseq)
            then go possibleSubseq bestsubseq (xs:rest)
            else go [] (if length subseq > length bestsubseq then subseq else bestsubseq) rest

removeAll :: Eq a => [a] -> [[a]] -> [[a]]
removeAll subseq = filter (not . null) . concatMap (split subseq)

compressMovementStrategy :: MovementStrategy -> [Integer]
compressMovementStrategy moveStrat =
    let
        foo = do x <- [15..20] -- just gonna kinda brute force this shit.
                 y <- [15..20] -- just, like... fuck you CPU.
                 z <- [15..20] -- broot, fuggin, force
                 let a = findSubsequence x [moveStrat]
                     aRemoved = removeAll a [moveStrat]
                     b = findSubsequence y aRemoved
                     abRemoved = removeAll b aRemoved
                     c = findSubsequence z abRemoved
                     abcRemoved = removeAll c abRemoved
                 guard $ null abcRemoved
                 pure (a,b,c)
        (a,b,c) = head foo -- YOLO!
        movementRoutine = fromIntegral . ord <$> intersperse ',' (buildMovementRoutine moveStrat (a,b,c) [])
    in intercalate [fromIntegral $ ord '\n']
        [ movementRoutine
        , movementStrategyToIntCodeWithCommas a
        , movementStrategyToIntCodeWithCommas b
        , movementStrategyToIntCodeWithCommas c
        ] ++ [fromIntegral $ ord '\n'] -- lol
    where
    -- TODO: this totally sucks, but whatevs, it works
    buildMovementRoutine [] _ acc = reverse acc
    buildMovementRoutine moveStrat (a,b,c) acc
        | startswith a moveStrat = buildMovementRoutine (drop (length a) moveStrat) (a,b,c) ('A':acc)
        | startswith b moveStrat = buildMovementRoutine (drop (length b) moveStrat) (a,b,c) ('B':acc)
        | startswith c moveStrat = buildMovementRoutine (drop (length c) moveStrat) (a,b,c) ('C':acc)
        | otherwise = error "well... that shouldn't have happened"

scaffoldIntersectionCount :: Program -> Int
scaffoldIntersectionCount asciiProg =
    let Program{ outputs_ = out } = runProgram asciiProg
        (scaffoldSet, _) = interpretAsciiOutput out
        intersections = findIntersections scaffoldSet
    in sum $ map (uncurry (*)) $ Set.toList intersections

getDustCount :: Program -> Integer
getDustCount asciiProg =
    let p@Program{ outputs_ = out } = runProgram asciiProg { intcodes_ = intcodes_ asciiProg // [(0,2)] }
        (scaffoldSet, bot) = interpretAsciiOutput out
        intersections = findIntersections scaffoldSet
        movementStrategy = findAPath scaffoldSet intersections bot
        compressedMovementStrategy = compressMovementStrategy movementStrategy
        inputs = compressedMovementStrategy ++ [fromIntegral $ ord 'n', fromIntegral $ ord '\n']
        p'@Program { outputs_ = out' } = runProgram (p { inputs_ = inputs, status_ = Running })
    in head out' -- lol, this is all kindsa shitty, but whatevs, YOLO
