{-# LANGUAGE MultiWayIf #-}

module Bug where

import Util

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad

data Bug = Bug | NotBug deriving (Eq, Show)

type RecursionLevel = Int

type BugBoard = Map.Map Point Bug

type RecursiveBugBoard = Map.Map (Point, RecursionLevel) Bug

charToBug :: Char -> Bug
charToBug '.' = NotBug
charToBug '#' = Bug

makeBugBoard :: [String] -> BugBoard
makeBugBoard xs = Map.fromList $ do
    (y, row) <- zip [0..] xs
    (x, bug) <- zip [0..] row
    pure ((x,y), charToBug bug)

bugInput :: BugBoard
bugInput = makeBugBoard
    [ ".#.#."
    , ".##.."
    , ".#..."
    , ".###."
    , "##..#"
    ]

recursiveBugInput :: RecursiveBugBoard
recursiveBugInput = makeRecursiveBugBoard
    [ ".#.#."
    , ".##.."
    , ".#..."
    , ".###."
    , "##..#"
    ]

isBug :: Bug -> Bool
isBug Bug = True
isBug _ = False

neighborBugs :: BugBoard -> Point -> BugBoard
neighborBugs bugBoard (x,y) =
    -- using NotBug because intersection is left biased
    let possibleNeighbors = Map.fromList [((x+x', y+y'), NotBug) | x' <- [-1..1], y' <- [-1..1], abs x' /= abs y' ]
    in Map.intersection bugBoard possibleNeighbors


evolveBugs :: BugBoard -> BugBoard
evolveBugs bugBoard =
    Map.mapWithKey liveOrDie bugBoard
    where
    liveOrDie :: Point -> Bug -> Bug
    liveOrDie location Bug = if Map.size (Map.filter isBug (neighborBugs bugBoard location)) == 1 then Bug else NotBug
    liveOrDie location NotBug =
        let adjacentBugs = Map.size (Map.filter isBug (neighborBugs bugBoard location))
        in
            if adjacentBugs == 1 || adjacentBugs == 2
            then Bug
            else NotBug

biodiversity :: BugBoard -> Integer
biodiversity bugBoard = sum $ do
    y <- [0..boardWidth]
    x <- [0..boardHeight]
    case Map.lookup (x,y) bugBoard of
        Just Bug -> pure $ fromIntegral $ 2 ^ (x + (y * (boardWidth + 1)))
        _ -> pure 0
    where
    boardWidth :: Int
    boardWidth = Set.findMax (Set.map fst $ Map.keysSet bugBoard)
    boardHeight :: Int
    boardHeight = Set.findMax (Set.map snd $ Map.keysSet bugBoard)

findRepeat :: BugBoard -> Integer
findRepeat = go Set.empty
    where
    go :: Set.Set Integer -> BugBoard -> Integer
    go seent bugBoard =
        let newBugBoard = evolveBugs bugBoard
            newBiodiversity = biodiversity newBugBoard
        in
            if Set.member newBiodiversity seent
            then newBiodiversity
            else go (Set.insert newBiodiversity seent) newBugBoard

makeRecursiveBugBoard :: [String] -> RecursiveBugBoard
makeRecursiveBugBoard xs =
    let groundZero = Map.fromList $ do (y, row) <- zip [0..] xs
                                       (x, bug) <- zip [0..] row
                                       guard $ (x,y) /= (2,2)
                                       pure (((x,y), 0), charToBug bug)
        otherLevels = Map.fromList $ do level <- [-200..200] -- add some slop JIC, lol
                                        guard $ level /= 0
                                        y <- [0..4]
                                        x <- [0..4]
                                        guard $ (x,y) /= (2,2)
                                        pure (((x,y), level), NotBug)
    in Map.union groundZero otherLevels

-- This solution is pretty bad... it does way more work than necessary, but whatevs, it works and isn't
-- infeasibly slow
recursiveNeighborBugs :: RecursiveBugBoard -> (Point, RecursionLevel) -> RecursiveBugBoard
recursiveNeighborBugs bugBoard ((x,y), level) =
    let possibleNeighbors = Map.fromList $ do  x' <- [-1..1]
                                               y' <- [-1..1]
                                               guard $ abs x' /= abs y'
                                               let newX = x + x'
                                                   newY = y + y'
                                               if | newX < 0 -> pure (((1,2), level - 1), NotBug)
                                                  | newX > 4 -> pure (((3,2), level - 1), NotBug)
                                                  | newY < 0 -> pure (((2,1), level - 1), NotBug)
                                                  | newY > 4 -> pure (((2,3), level - 1), NotBug)
                                                  | (newX,newY) == (2,2) ->
                                                        if | (x,y) == (2,1) -> [(((innerX,0), level + 1), NotBug) | innerX <- [0..4]]
                                                           | (x,y) == (1,2) -> [(((0,innerY), level + 1), NotBug) | innerY <- [0..4]]
                                                           | (x,y) == (2,3) -> [(((innerX,4), level + 1), NotBug) | innerX <- [0..4]]
                                                           | (x,y) == (3,2) -> [(((4,innerY), level + 1), NotBug) | innerY <- [0..4]]
                                                  | otherwise -> pure (((newX,newY), level), NotBug)
    in Map.intersection bugBoard possibleNeighbors

recursiveBugCount :: RecursiveBugBoard -> Integer
recursiveBugCount = fromIntegral . Map.size . Map.filter isBug

evolveBugsRecursively :: RecursiveBugBoard -> RecursiveBugBoard
evolveBugsRecursively bugBoard =
    Map.mapWithKey liveOrDie bugBoard
    where
    liveOrDie :: (Point, Int) -> Bug -> Bug
    liveOrDie location Bug = if Map.size (Map.filter isBug (recursiveNeighborBugs bugBoard location)) == 1 then Bug else NotBug
    liveOrDie location NotBug =
        let adjacentBugs = Map.size (Map.filter isBug (recursiveNeighborBugs bugBoard location))
        in
            if adjacentBugs == 1 || adjacentBugs == 2
            then Bug
            else NotBug

iterateRecursiveBugBoard :: Int -> RecursiveBugBoard -> RecursiveBugBoard
iterateRecursiveBugBoard 0 bugBoard = bugBoard
iterateRecursiveBugBoard n bugBoard = iterateRecursiveBugBoard (n - 1) (evolveBugsRecursively bugBoard)

bugPart2Solution :: Integer
bugPart2Solution =
    let recursiveBoardEvolved = iterateRecursiveBugBoard 200 recursiveBugInput
    in recursiveBugCount recursiveBoardEvolved
