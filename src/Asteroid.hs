module Asteroid where

import Util

import qualified Data.Set as Set
import Data.List
import Control.Monad
import Data.Function (on)

------------
-- Day 10 --
------------

type AsteroidField = Set.Set Point

data Sign = Pos | Neg deriving (Show, Eq, Ord)

type Angle = (Maybe Rational, Sign, Sign)

listToAsteroidField :: [String] -> AsteroidField
listToAsteroidField xs = Set.fromList $ do
    (y, row) <- zip [0..] xs
    (x, cell) <- zip [0..] row
    guard $ cell == '#'
    pure (x,y)

sign x = if x < 0 then Neg else Pos

angle :: Point -> Point -> Angle
angle (x1, y1) (x2, y2) =
    let dx = x2 - x1
        dy = y2 - y1
        slope = toRational dy / toRational dx
    in (if dx == 0 then Nothing else Just slope, sign dx, sign dy)

radius :: Point -> Point -> Float
radius (x1, y1) (x2, y2) =
    let
        a = fromRational $ toRational $ x2 - x1
        b = fromRational $ toRational $ y2 - y1
        x = (a ** 2 + b ** 2)
    in sqrt x

visible :: AsteroidField -> Point -> Int
visible asteroidField p =
    Set.size $ Set.map (angle p) $ Set.delete p asteroidField

findMonitoringStation :: AsteroidField -> Point
findMonitoringStation asteroidField =
    snd $ maximumBy (\(n1, a1) (n2, a2) -> compare n1 n2) $ map (\a -> (visible asteroidField a, a)) $ Set.toList asteroidField

asteroidsDetectableFromBestLocation :: AsteroidField -> Int
asteroidsDetectableFromBestLocation asteroidField = visible asteroidField $ findMonitoringStation asteroidField

clockOrder (_, (s1, Pos, Neg)) (_, (s2, Pos, Neg)) = compare s1 s2
clockOrder (_, (_ , Pos, Neg)) _                   = LT
clockOrder _                   (_, (_, Pos, Neg))  = GT
clockOrder (_, (s1, Pos, Pos)) (_, (s2, Pos, Pos)) = compare s1 s2
clockOrder (_, (_ , Pos, Pos)) _                   = LT
clockOrder _                   (_, (_, Pos, Pos))  = GT
clockOrder (_, (s1, Neg, Pos)) (_, (s2, Neg, Pos)) = compare s1 s2
clockOrder (_, (_ , Neg, Pos)) _                   = LT
clockOrder _                   (_, (_, Neg, Pos))  = GT
clockOrder (_, (s1, Neg, Neg)) (_, (s2, Neg, Neg)) = compare s1 s2

vaporizeOrder :: Point -> AsteroidField -> [Point]
vaporizeOrder p asteroidField =
    unwrap
    $ map (map (fst . fst))
    $ groupBy ((==) `on` snd)
    $ sortBy clockOrder
    $ sortBy (compare `on` (snd . fst))
    $ map (\pt -> ((pt, radius p pt), angle p pt))
    $ Set.toList
    $ Set.delete p asteroidField
    where
    unwrap [] = []
    unwrap ((r:iR):oR) = r:unwrap (oR ++ [iR])
    unwrap ([]:oR) = unwrap oR

asteroidField = listToAsteroidField ["#....#.....#...#.#.....#.#..#....#", "#..#..##...#......#.....#..###.#.#", "#......#.#.#.....##....#.#.....#..", "..#.#...#.......#.##..#...........", ".##..#...##......##.#.#...........", ".....#.#..##...#..##.....#...#.##.", "....#.##.##.#....###.#........####", "..#....#..####........##.........#", "..#...#......#.#..#..#.#.##......#", ".............#.#....##.......#...#", ".#.#..##.#.#.#.#.......#.....#....", ".....##.###..#.....#.#..###.....##", ".....#...#.#.#......#.#....##.....", "##.#.....#...#....#...#..#....#.#.", "..#.............###.#.##....#.#...", "..##.#.........#.##.####.........#", "##.#...###....#..#...###..##..#..#", ".........#.#.....#........#.......", "#.......#..#.#.#..##.....#.#.....#", "..#....#....#.#.##......#..#.###..", "......##.##.##...#...##.#...###...", ".#.....#...#........#....#.###....", ".#.#.#..#............#..........#.", "..##.....#....#....##..#.#.......#", "..##.....#.#......................", ".#..#...#....#.#.....#.........#..", "........#.............#.#.........", "#...#.#......#.##....#...#.#.#...#", ".#.....#.#.....#.....#.#.##......#", "..##....#.....#.....#....#.##..#..", "#..###.#.#....#......#...#........", "..#......#..#....##...#.#.#...#..#", ".#.##.#.#.....#..#..#........##...", "....#...##.##.##......#..#..##...."]

asteroidField2 = listToAsteroidField
    [ ".#..#"
    , "....."
    , "#####"
    , "....#"
    , "...##"
    ]

testeroid = listToAsteroidField [".#..##.###...#######", "##.############..##.", ".#.######.########.#", ".###.#######.####.#.", "#####.##.#.##.###.##", "..#####..#.#########", "####################", "#.####....###.#.#.##", "##.#################", "#####.##.###..####..", "..######..##.#######", "####.##.####...##..#", ".#####..#.######.###", "##...#.##########...", "#.##########.#######", ".####.#.###.###.#.##", "....##.##.###..#####", ".#.#.###########.###", "#.#.#.#####.####.###", "###.##.####.##.#..##"]

runAsteroid :: IO ()
runAsteroid = do
  assert (asteroidsDetectableFromBestLocation asteroidField == 267) $ pure $ asteroidsDetectableFromBestLocation asteroidField
  assert (vaporizeOrder (26,28) asteroidField !! 199 == (13,6)) $ pure $ vaporizeOrder (26,28) asteroidField !! 199
  pure ()