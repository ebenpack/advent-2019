{-# LANGUAGE NegativeLiterals #-}

module TruffleShuffle where

import Data.List
import qualified Data.Vector as V

type SpaceCards = V.Vector Int

spaceCards :: SpaceCards
spaceCards = V.fromList [0..10006]

crazySpaceCards :: SpaceCards
crazySpaceCards = V.fromList [0..119315717514047]

truffleShuffleProcess :: [SpaceCards -> SpaceCards]
truffleShuffleProcess =
    [ cutN -45
    , incrementN 28
    , cutN -9687
    , incrementN 47
    , cutN 7237
    , incrementN 12
    , cutN -9336
    , incrementN 72
    , cutN -9471
    , newStack
    , cutN -3034
    , incrementN 5
    , cutN -5333
    , incrementN 69
    , cutN -3998
    , incrementN 20
    , cutN 1217
    , incrementN 40
    , cutN -421
    , newStack
    , cutN 6883
    , incrementN 7
    , cutN 1897
    , incrementN 57
    , cutN -3069
    , incrementN 10
    , cutN -5522
    , incrementN 64
    , cutN 1422
    , incrementN 55
    , cutN 973
    , incrementN 57
    , cutN 1061
    , incrementN 60
    , cutN -5652
    , incrementN 9
    , cutN 2037
    , incrementN 73
    , newStack
    , cutN -8727
    , incrementN 59
    , cutN -2227
    , newStack
    , cutN 467
    , incrementN 39
    , newStack
    , incrementN 67
    , cutN -3708
    , newStack
    , cutN 1394
    , incrementN 42
    , cutN -6618
    , incrementN 21
    , newStack
    , cutN 9107
    , incrementN 33
    , cutN 892
    , incrementN 32
    , newStack
    , cutN -7566
    , incrementN 45
    , cutN 5149
    , incrementN 53
    , newStack
    , incrementN 71
    , cutN -1564
    , incrementN 68
    , cutN 6372
    , incrementN 2
    , cutN -3799
    , incrementN 39
    , cutN -2830
    , incrementN 63
    , cutN -4758
    , incrementN 38
    , cutN -6179
    , incrementN 16
    , cutN -1023
    , newStack
    , incrementN 34
    , cutN -8829
    , incrementN 70
    , cutN 9112
    , incrementN 72
    , cutN -4044
    , incrementN 29
    , cutN 3010
    , incrementN 48
    , cutN -9025
    , incrementN 72
    , cutN -8418
    , incrementN 45
    , cutN -4991
    , incrementN 19
    , cutN -6999
    , incrementN 11
    , cutN 1852
    , incrementN 56
    , newStack
    , incrementN 39
    ]


newStack :: SpaceCards -> SpaceCards
newStack = V.reverse

incrementN :: Int -> SpaceCards -> SpaceCards
incrementN n cards = go 0 0 (V.fromList [ -1 | _ <- [0..V.length cards - 1] ])
    where
    go m p acc
        | p >= V.length cards = acc
        | acc V.! m /= -1 = acc
        | otherwise =
            let newAcc = acc V.// [(m, cards V.! p)]
                newN = (m + n) `mod` V.length cards
            in go newN (p + 1) newAcc

cutN :: Int -> SpaceCards -> SpaceCards
cutN n cards =
    if n < 0
    then
        let (a,b) = V.splitAt (V.length cards + n) cards
        in b V.++ a
    else
        let (a,b) = V.splitAt n cards
        in b V.++ a

testshuf1 = incrementN 7 (V.fromList [0..9])

truffleShuffle :: [SpaceCards -> SpaceCards]  -> SpaceCards -> SpaceCards
truffleShuffle truffleShufProc cards =
    foldl' (\newCards f -> f newCards) cards truffleShufProc

truffleShuffleNTimes :: Int -> [SpaceCards -> SpaceCards]  -> SpaceCards -> SpaceCards
truffleShuffleNTimes 0 _ cards = cards
truffleShuffleNTimes n truffleShufProc cards = truffleShuffleNTimes (n - 1) truffleShufProc (truffleShuffle truffleShufProc cards)

truffleShuffleAnswer1 :: Maybe Int
truffleShuffleAnswer1 =
    let shuffled = truffleShuffle  truffleShuffleProcess spaceCards
    in V.findIndex (== 2019) shuffled

truffleShuffleAnswer2 :: Int -> Int
truffleShuffleAnswer2 n =
    if n == 0 then error "uh oh"
    else 
        let shuffled = truffleShuffle truffleShuffleProcess crazySpaceCards
        in 
            if shuffled == crazySpaceCards
            then n
            else truffleShuffleAnswer2 (n - 1)

testcards = V.fromList [0..9]

testresc = truffleShuffle [newStack, cutN -2, incrementN 7, cutN 8, cutN -4, incrementN 7, cutN 3, incrementN 9, incrementN 3, cutN -1] testcards

truftest1 = (truffleShuffleNTimes 3 truffleShuffleProcess spaceCards) V.! 2020