{-# LANGUAGE FlexibleContexts #-}

module Maze where

import Util

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Loops
import Control.Parallel.Strategies
import Control.Monad.State
import Codec.Picture (PixelRGBA8(..), generateImage)
import Codec.Picture.Bitmap (writeBitmap)

mazeInput :: [String]
mazeInput =
    [ "#################################################################################"
    , "#.............#...#...O.#.#...........#.#...#.........#.......#.....#.......#.Z.#"
    , "#####.#######.#H#.#.###.#.#.#####.###.#.#.###.#####.#.#.#####.#.###.#.###.###.#.#"
    , "#.....#.#...#.#.#...#.#.#.#.#.#...#.....#.#...#...#.#.#...#w..#...#...#.#.....#.#"
    , "#.#####.#.#.#.#.#####.#.#.#B#.#.#########.#.###.#.#.#####.#.#####.#####.#######.#"
    , "#.#...#...#.....#...#.#.#.....#.........#.#.#...#.#.....#.#...#..y..#...#.....#.#"
    , "#.#.#.#.#########.#.#.#.###########.###.#.#.#.#######.###.###.#####.#.#.#.###.#.#"
    , "#.#.#...#.........#.#.......#.....#...#.#.#.#.#.....#.....#.#.#...#...#.#.#...#.#"
    , "#.###.###.#########.#######.#.###.#####.#.#.#.#.###.#######.#.#.#.#####.#.#.###.#"
    , "#...#.#.....#x....#.#..f..#...#...#...#.#.#.#.....#.....#i..#...#.#...#.#.#.#...#"
    , "#.#.#.#.#####.###.#.#A###.#####.###.#.#.#.#.#########.#.#.#.#####.#.#.###.#.#.#.#"
    , "#.#.#.#.#...#.#.#.#...#.#.#...#.#.T.#...#.#...#.....#.#...#.#.#..e#.#.....#.#.#.#"
    , "###.###.#X#.#.#.#.#.###.#.###.#.###.###.#.###.#.###.#.#####.#.#.###.#######.#.#.#"
    , "#...#d..#.#...#.#.#.....#...#.#...#...#.#...#.#.#.....#.......#.#.....#.#...#.#.#"
    , "#.###.###.#####R#.#####.###.#.###.###.#.#.#.#.#.###########.###.#.###.#.#.#####.#"
    , "#...#.#.#...#.......#...#...#...#.....#.#.#...#.....#.....#.#...#...#.#...#...#.#"
    , "###E#.#.###.#######.#####.###.#.#######.#.#####.###N#.###.###.###.###.#.###.#.#.#"
    , "#.#...#...#..c..#...#r..F.#...#.....#.#.#...#.#.#.#.#...#.#...#...#...#.....#.#.#"
    , "#.#####.#######.###.#.#######.#####.#.#.###.#.#.#.#.###.#.#.###.###.#########.#.#"
    , "#z....#.......#...#...#.....#s#.......#.#.#.#.....#...#.#...#v..#.#...#.....#...#"
    , "#.#.###.#.#######.#####G###.#.#######.#.#.#.#####.#.###.#######.#.###.#.#######.#"
    , "#.#.#...#.......#...#...#.#.#...#...#.#.#.#.....#.#...#.D.....#.....#.#.......#.#"
    , "###.#.#########.###C#####.#.#.#.#.#.###.#.#####.#####.###############.#.#####.#.#"
    , "#...#...#.....#...#....g....#.#.#.#.....#.....#.....#...........#.....#.....#...#"
    , "#.#####.#.###.#.#############.#.#.#######.#########.#.#######.#.#.#########.#####"
    , "#.....#...#...#.#.........#...#.#.#.....#.........#.#.#.....#.#...#.......#.#..u#"
    , "#Q#.#######.###.#.#.#.#####.###.#.#####.#.#######.#.###.###.#.#####.###.#.#.#.###"
    , "#.#.......#.#...#.#.#.#...#.#.#...#...#.#.#.....#.#.#...#.#.#...#.....#.#.#.#...#"
    , "#.#####.###.#.###.#.###.#.#.#.#####.#.#.#.#.###.#.#.#.###.#.#.#.#######.#.#.#.#.#"
    , "#...#...#...#.....#.....#...#.......#...#.#.#.#.K.#...#.#...#.#.#.......#.#.#.#.#"
    , "###.#.###.#####################.###.#####.#.#.#.#######.#.#####.#.#####.###.#.#.#"
    , "#...#.....#...#.....#.........#...#.#...#.#.#...#.......#...#...#...#...#...#.#.#"
    , "#.#########.###.###.#.###.###.###.#.#.#.###.#####.#.###.###.#.###.#.#.###.#####.#"
    , "#.#.....#.........#...#...#.#.#.#.#.#.#.#...#.....#.#l..#.#.#...#.#.#...#...#...#"
    , "#.###.#.###.###########.###.#.#.#.#.#.#.#.###.#####.#.###.#.###.#.#.#.#####.#.#.#"
    , "#.#...#...#...#...#.....#...#.#...#...#.#...#.#.#...#.....#.....#.#.#.#...#.#.#.#"
    , "#.#.#####.#.###.#.#.#####.###.#########.#.#.#.#.#.#################.###.#.#P#.#.#"
    , "#.#.#.....#.#...#.#...#.#...#.#.....#...#.#.#.#.#.#.................#...#...#.#.#"
    , "#.#.#.#######.###.###.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#########.#######.#######.#.#"
    , "#.U.#p........#....j..#...#.....#.....#...#.....#...........#...........J.....#.#"
    , "#######################################.@.#######################################"
    , "#...#.....#...#.......#.........#.........#.....#.....#.....#.....#.............#"
    , "#.#.#.###.###.#.#L###.#.#####.###.#.###.#.#.#.###.#.###.#.#.#.#.#.#.#########.#.#"
    , "#.#...#.....#...#...#.#.#...#.....#...#.#...#.....#.....#.#.#.#.#.#.#.......#q#.#"
    , "#.#.#######.#.#####.#.###.#.#########.#.#.###############.###.#.#.###.#####.###.#"
    , "#.#.#.....#.#...#...#.....#.#.........#.#.#.....#.....#...#...#.#.....#...#...#.#"
    , "#.#.#.###.#.#####.#########.#.#########.#.#.#.#.#.###.#.###.###.#.#####.#.###.#.#"
    , "#.#.#...#.#.....#.....#...#...#.......#.#.#.#.#...#...#.#.....#.#.#...#.#...#...#"
    , "#.#####.#.#####.#####.#.#######.###.###.#.#.#.#####.###.#.#####.#.#.#.#.###.###.#"
    , "#.....#.#...#...#...#.#...........#...#.#k#.#...#.....#.#.#.....#.#.#.#...#...#.#"
    , "#####.#.###.###.#.#.#.#.#############.#.#.#.###.#.#####.#.#.#######.#.###.#.###.#"
    , "#.....#...#...#...#...#.#...........#...#.#...#.#...#...#.#...#...#.#...#.#.....#"
    , "#.#######.###.#####.#####.#########.#.###.#####.###.#.###.###.#.#.#.###.#.#######"
    , "#.......#...#.....#.#.....#.......#.#...#.......#.#.#.#...#.#...#.....#.#.#.....#"
    , "#.###.#####.#####.###.#####.#.#.###.#############.#.#.#.###.###########.#.#.###.#"
    , "#...#.#.........#...#.#.....#.#.#...#...#...#.....#.#...#...#.......#...#.#.#m..#"
    , "#.###.#.###########.#.#######.###.###.#.#.#.#.#.###.#####.#.#######.#.###.#.#.#.#"
    , "#.#...#...#.#.......#...#...#.W...#...#.#.#.#.#.....#.....#...#.....#...#.#.#.#.#"
    , "###.#.###.#.#.#########.#.#.#####.###.#.#.###.#######.#######.#.###.###.#.###.#.#"
    , "#...#...#...#...#.....#...#.....#...#.#.#.#...#.........#.......#.#.#...#.....#.#"
    , "#.#########.###.#.#.###########.###.#.#.#.#.###.#######.#.#######.#.#.#########.#"
    , "#.........#.#...#.#.#....a......#.#...#.#.#...#.#.......#.#...#.....#.#...#.....#"
    , "#.#######.#.#.###.#.#############.#####.#.###.#.#.#######.#.###.#####.###.#.#####"
    , "#.#.....#...#.#...#.....#...#.......#h..#.....#.#.#.......#.#...#...#...#.#.#...#"
    , "#.###.#.#####.#.#####.#.#.#.#.#####.#.###.#######.#######.#.#####.#.###.#.#.#.#.#"
    , "#...#.#...#...#.#...#.#.#.#..o#...#...#.#.......#.......#.#.....#.#.#...#.....#.#"
    , "###.#####.#.#.#.###.#.###.#######.#####.#######.#.#####.#######.#.#.#.###########"
    , "#.#...#...#.#.#.....#.....#.#.......#...#...#...#.....#.......#.#.#.#...#.......#"
    , "#.###.#.###.#.#####.#######.#.#I###.###.#.#.#.###########.###.#.#.#.###.#.#####.#"
    , "#...#...#...#.....#.#.#.....#.#...#t..#.#.#...#.........#.#.#.#...#...#...#.....#"
    , "#.#####.#.#########.#.#.###.#.###.###.#.#.#####.#######.#.#.#.#######.#####.#####"
    , "#...M...#.......#...#...#.#...#.#...#...#.......#.....#.#.#.........#.....#.#...#"
    , "#.#############.#.###.###.#####.###.#############.#####.#.#.#######.#####.#.###.#"
    , "#.#.....Y...#.#.#.#.#.#..b..#...#.#.#...#...#.........#.#.#.#...#.#.#...#...#...#"
    , "#.#.#######.#.#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.#######.#.#.###.#.#.#.#.#.#####.#.#"
    , "#...#n....#.#.#...#.......#...#...#...#.#.#.#.#.....#...#.....#.#...#.#.......#.#"
    , "#####.###.#.#.###################.#####.#.###.#.###.#####V#####.#.###.###.#####.#"
    , "#.....#...#...#.....#...#...#.....#.....#.....#...#...........#.#...#.#...#.....#"
    , "#.###########.###.#.#.#.#.#.#######.###.#.#####################.###.###.###.#####"
    , "#.................#...#...#.........#.S.#.......................#.......#.......#"
    , "#################################################################################"
    ]

data MazeTile = MazeFloor | MazeWall | MazeGuy | MazeKey Char | MazeDoor Char deriving (Eq, Ord, Show)

type Maze = Map.Map Point MazeTile

isKey :: MazeTile -> Bool
isKey (MazeKey _) = True
isKey _ = False

isFloor :: MazeTile -> Bool
isFloor MazeFloor = True
isFloor _ = False

isGuy :: MazeTile -> Bool
isGuy MazeGuy = True
isGuy _ = False

isWall :: MazeTile -> Bool
isWall MazeWall = True
isWall _ = False

isDoor :: MazeTile -> Bool
isDoor (MazeDoor _) = True
isDoor _ = False

keyFits :: MazeTile -> MazeTile -> Bool
keyFits (MazeKey k) (MazeDoor d) = k == d
keyFits _ _ = False

charToTile :: Char -> MazeTile
charToTile '.' = MazeFloor
charToTile '#' = MazeWall
charToTile '@' = MazeGuy
charToTile c
    | isLower c = MazeKey (toUpper c)
    | otherwise = MazeDoor (toUpper c) -- YOLO!

makeMaze :: [String] -> Maze
makeMaze xs = Map.fromList $ do
    (y, row) <- zip [0..] xs
    (x, cell) <- zip [0..] row
    pure ((x,y), charToTile cell)

findMazeNeighbors :: Maze -> Point -> (MazeTile -> Bool) -> Set.Set Point
findMazeNeighbors maze (x,y) isNavigable = Set.fromList $ do
    x' <- [-1..1]
    y' <- [-1..1]
    guard $ abs x' /= abs y'
    let val = Map.lookup (x + x', y + y') maze
    guard $ maybe False isNavigable val
    pure (x + x', y + y')

findShortestMazePathBetween :: Maze -> Point -> Point -> [Point]
findShortestMazePathBetween maze source destination = go [[source]] (Set.singleton source)
    where
    isNavigable :: MazeTile -> Bool
    isNavigable = not . isWall
    go :: [[Point]] -> Set.Set Point -> [Point]
    go queue discovered
        | null queue = [] -- no path found
        | otherwise =
            let currentPath = last queue
                currentNode = head currentPath
                neighbors = Set.difference (findMazeNeighbors maze currentNode isNavigable) discovered
                newlyDiscovered = Set.union discovered neighbors
                neighborList = Set.toList neighbors
                newQueue = map (: currentPath) neighborList ++ init queue
            in
                if currentNode == destination
                then reverse $ tail currentPath
                else go newQueue newlyDiscovered

precalculateConnections :: Maze -> Map.Map (Point, Point) (Int, Set.Set Char)
precalculateConnections maze = Map.fromList $
    do  let pointsOfInterest = Map.toList (Map.filter (\x -> isDoor x || isKey x || isGuy x) maze)
        (p1, _) <- pointsOfInterest
        (p2, _) <- pointsOfInterest
        guard $ p1 /= p2
        let path = findShortestMazePathBetween maze p1 p2
            doorsBetween = Set.fromList
                $ map ((\(MazeDoor c) -> c) . (\p -> maze Map.! p))
                $ filter (\p -> maybe False isDoor (Map.lookup p maze)) path
        guard $ not $ null path
        [ ((p1, p2), (length path, doorsBetween)), ((p2, p1), (length path, doorsBetween))]

findAllPathsThroughMaze :: Maze -> Int
findAllPathsThroughMaze maze = evalState (go startingLocs Set.empty allKeys) Map.empty
    where
    keys :: Map.Map Char Point
    keys =
        let keys' = Map.toList (Map.filter isKey maze)
        in Map.fromList $ map (\(a, MazeKey c) -> (c, a)) keys'
    startingLocs :: Set.Set Point
    startingLocs = Set.fromList $ fst <$> Map.toList (Map.filter isGuy maze)
    getLeyLoc :: Char -> Point
    getLeyLoc key = keys Map.! key
    connections :: Map.Map (Point, Point) (Int, Set.Set Char)
    connections = precalculateConnections maze
    canAccess :: Set.Set Char -> Set.Set Char -> Bool
    canAccess doorsBetween keys = Set.null $ doorsBetween Set.\\ keys
    allKeys :: Set.Set Char
    allKeys = Set.fromList $ map (\(MazeKey c) -> c) $ Map.elems $ Map.filter isKey maze
    -- this slow as shit, esp. for part 2. whatevs, gets there in the end
    go currLocs keysHeld keysNeeded
        | Set.null keysNeeded = pure 0
        | otherwise =
            let reachableKeys = [ (currLoc, keyLoc, key, distance) -- lol, this ugly... YOLO!
                                | key <- Set.toList keysNeeded
                                , currLoc <- Set.toList currLocs
                                , let keyLoc = getLeyLoc key
                                , let (distance, doorBetween) = connections Map.! (currLoc, keyLoc)
                                , Map.member (currLoc, keyLoc) connections
                                , canAccess doorBetween keysHeld
                                ]
            in fmap minimum $ forM reachableKeys $ \(currLoc, keyLoc, key, distance) -> do
                let newKeysHeld = Set.insert key keysHeld
                    newKeysNeeded = Set.delete key keysNeeded
                    memoKey = (currLocs, keyLoc, keysHeld)
                    newLocs = Set.insert keyLoc (Set.delete currLoc currLocs) -- YOLO
                memo <- get
                -- (8,24,32,72)
                case Map.lookup memoKey memo of
                    Just v -> pure $ v + distance
                    Nothing -> do
                        result <- go newLocs newKeysHeld newKeysNeeded
                        modify $ Map.insert memoKey result
                        pure $ result + distance

tst3 =
    [ "########"
    , "#..@.Aa#"
    , "########"
    ]

testmaze1 =
    [ "########################"
    , "#f.D.E.e.C.b.A.@.a.B.c.#"
    , "######################.#"
    , "#d.....................#"
    , "########################"
    -- 012345678901234567890123
    ]
testmaze2 =
    [ "########################"
    , "#...............b.C.D.f#"
    , "#.######################"
    , "#.....@.a.B.c.d.A.e.F.g#"
    , "########################"
    ]
testmaze3 =
    [ "#################"
    , "#i.G..c...e..H.p#"
    , "########.########"
    , "#j.A..b...f..D.o#"
    , "########@########"
    , "#k.E..a...g..B.n#"
    , "########.########"
    , "#l.F..d...h..C.m#"
    , "#################"
    ]
testmaze4 =
    [ "########################"
    , "#@..............ac.GI.b#"
    , "###d#e#f################"
    , "###A#B#C################"
    , "###g#h#i################"
    , "########################"
    ]

testmazeinputs =
    [ testmaze1
    , testmaze2
    , testmaze3
    , testmaze4
    ]


replaceGuys :: Maze -> Maze
replaceGuys maze =
    let (x,y) = fst $ head $ Map.toList (Map.filter isGuy maze) -- assuming there is exactly one guy on the map
        updatedGuyLocs = Map.fromList $ do
            x' <- [-1..1]
            y' <- [-1..1]
            let newTile = if abs x' == abs y' && x' /= 0 then MazeGuy else MazeWall
            pure ((x + x', y + y'), newTile)
        newMaze = Map.union updatedGuyLocs maze
    in newMaze

renderMaze :: Maze -> IO ()
renderMaze maze = do
    let image = generateImage writePixel mazeWidth mazeHeight
    writeBitmap "./maze.bmp" image
    where
    mazeWidth = 1 + Set.findMax (Set.map fst $ Map.keysSet maze)
    mazeHeight = 1 + Set.findMax (Set.map snd $ Map.keysSet maze)
    writePixel x y =
        let color =  (maze Map.! (x,y)) -- YOLO!
        in
            case color of
                MazeFloor -> PixelRGBA8  0   0   0   255 -- black
                MazeGuy -> PixelRGBA8    255 255 255 255 -- white
                MazeWall -> PixelRGBA8   255 0   0   255 -- red
                MazeDoor _ -> PixelRGBA8 0   255 0   255 -- green
                MazeKey _ -> PixelRGBA8  0   0   255 255 -- blue

printMaze :: Maze -> IO ()
printMaze maze = do
    forM_ [ (x,y) | y <- [0..mazeHeight], x <- [0..mazeWidth] ]
          (\(x,y) -> (if x == 0 then putStr "\n" else pure ()) >> printTile (maze Map.! (x,y)))
    putStr "\n\n"
    where
    mazeWidth = Set.findMax $ Set.map fst $ Map.keysSet maze
    mazeHeight = Set.findMax $ Set.map snd $ Map.keysSet maze
    printTile MazeWall = putStr "#"
    printTile MazeGuy = putStr "@"
    printTile MazeFloor = putStr "."
    printTile (MazeDoor d) = putStr [toUpper d]
    printTile (MazeKey k) = putStr [toLower k]

puzzleMaze :: Maze
puzzleMaze = makeMaze mazeInput

part1MazeAnswer :: Int
part1MazeAnswer = findAllPathsThroughMaze (makeMaze mazeInput)

anothertest1 = makeMaze
    [ "#############"
    , "#DcBa.#.GhKl#"
    , "#.###@#@#I###"
    , "#e#d#####j#k#"
    , "###C#@#@###J#"
    , "#fEbA.#.FgHi#"
    , "#############"
    ]
anothertest2 = makeMaze -- should be 72! not 74!
    [ "#############"
    , "#g#f.D#..h#l#"
    , "#F###e#E###.#"
    , "#dCba@#@BcIJ#"
    , "#############"
    , "#nK.L@#@G...#"
    , "#M###N#H###.#"
    , "#o#m..#i#jk.#"
    , "#############"
    ]
testblah = findAllPathsThroughMaze $ replaceGuys newtest1

part2MazeAnswer :: Int
part2MazeAnswer = -- 2546 NO! 2462 NO! TOO HIGH! | 1934 NO! TOO LOW!, 1960 NO!
    let newmaze = replaceGuys (makeMaze mazeInput)
    in findAllPathsThroughMaze newmaze

newtest1 = makeMaze -- should be 24
    [ "###############"
    , "#d.ABC.#.....a#"
    , "######@#@######"
    , "###############"
    , "######@#@######"
    , "#b.....#.....c#"
    , "###############"
    ]

p2test1 = makeMaze
    [ "#######"
    , "#a.#Cd#"
    , "##@#@##"
    , "#######"
    , "##@#@##"
    , "#cB#Ab#"
    , "#######"
    ]

p2test2 = makeMaze
    [ "###############"
    , "#d.ABC.#.....a#"
    , "######@#@######"
    , "###############"
    , "######@#@######"
    , "#b.....#.....c#"
    , "###############"
    ]

p2test3 = makeMaze
    [ "#############"
    , "#DcBa.#.GhKl#"
    , "#.###@#@#I###"
    , "#e#d#####j#k#"
    , "###C#@#@###J#"
    , "#fEbA.#.FgHi#"
    , "#############"
    ]

p2test4 = makeMaze
    [ "#############"
    , "#g#f.D#..h#l#"
    , "#F###e#E###.#"
    , "#dCba@#@BcIJ#"
    , "#############"
    , "#nK.L@#@G...#"
    , "#M###N#H###.#"
    , "#o#m..#i#jk.#"
    , "#############"
    ]

testgg =
    let aaa = findAllPathsThroughMaze p2test1
        bbb = findAllPathsThroughMaze p2test2
        ccc = findAllPathsThroughMaze p2test3
        ddd = findAllPathsThroughMaze p2test4
    in (aaa,bbb,ccc,ddd) -- should be (8,24,32,72)