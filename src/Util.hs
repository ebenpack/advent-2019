{-# LANGUAGE DeriveFoldable #-}

module Util where

import Data.List
import Data.Char

data Tree a = Empty | Tree { node_ :: a, children_ :: [Tree a] } deriving (Eq, Show, Foldable)

type Point = (Int,Int)

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs

leftPad :: Int -> [Integer] -> [Integer]
leftPad n xs = replicate (n - length ys) 0 ++ ys
    where ys = take n xs
    
next :: (Eq a, Enum a, Bounded a) => a -> a
next e | e == maxBound = minBound
       | otherwise = succ e

intToIntList :: Integral a => a -> [a]
intToIntList n = go n []
    where
    go n acc
        | n < 10 = n:acc
        | otherwise = go (n `div` 10) (n `mod` 10:acc)

intListToInt :: Integral a => [a] -> a
intListToInt = foldl' (\a b -> a * 10 + b) 0

intToString :: Integral a => a -> String
intToString = map (intToDigit . fromIntegral) . intToIntList

-- tree junk

-- TODO: there's probably a more general way to solve both of these problems
findShortestPathLength :: Eq a => Tree a -> a -> Int
findShortestPathLength tree destination = go tree 0
    where
    go Empty _ = 0
    go Tree { node_ = node, children_ = children } depth =
        if node == destination
        then depth
        else sum $ map (`go` (depth + 1)) children

--foldTree :: Eq a => (a -> b -> b) -> b -> Tree a -> a -> b
--foldTree f def tree destination = go tree def
--    where
--    go Empty def = def
--    go Tree { node_ = node, children_ = children } def =
--        if node == destination
--        then f node def
--        else foldl' (\d t -> go t (f node d)) def children

findLongestPathLength :: Tree a -> Int
findLongestPathLength tree = go tree 0
    where
    go Empty depth = depth
    go Tree { node_ = node, children_ = children } depth =
        if null children
        then depth
        else maximum $ map (`go` (depth + 1)) children
        
testtree1 = 
    Tree 1 [Tree 2 [Tree 3 []]] 
      
fftest = foldl (\tot _ -> tot + 1) 0 testtree1

-- (b -> a -> b) -> b -> t a -> b
