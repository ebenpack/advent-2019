module Password where

import Util

import Data.List

------------
-- Day 04 --
------------



twoAdjacent :: [Int] -> Bool
twoAdjacent = any ((>= 2) . length) . group

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:y:zs) = (x, y) : pairs (y : zs)

nonDecreasing :: [Int] -> Bool
nonDecreasing = all (uncurry (<=)) . pairs

initialPasswordCriteria :: Int -> Bool
initialPasswordCriteria n
    | length xs /= 6         = False
    | not $ twoAdjacent xs   = False
    | not $ nonDecreasing xs = False
    | otherwise              = True
    where
    xs = intToIntList n
    
noGroupedTwoAdjacent :: [Int] -> Bool
noGroupedTwoAdjacent xs = any ((== 2) . length) (group xs)
    
passwordInputs :: [Int]
passwordInputs = [171309..643603]

passwordMeetingInitialCriteria :: [Int]
passwordMeetingInitialCriteria = [x | x <- passwordInputs, initialPasswordCriteria x]

numPasswordMeetingInitialCriteria :: Int
numPasswordMeetingInitialCriteria = length passwordMeetingInitialCriteria

passwordMeetingAllCriteria :: [Int]
passwordMeetingAllCriteria = [x | x <- passwordMeetingInitialCriteria, noGroupedTwoAdjacent $ intToIntList x]

numPasswordMeetingAllCriteria :: Int
numPasswordMeetingAllCriteria = length passwordMeetingAllCriteria

runPassword :: IO ()
runPassword = do
  assert (numPasswordMeetingInitialCriteria == 1625) $ pure numPasswordMeetingInitialCriteria
  assert (numPasswordMeetingAllCriteria == 1111) $ pure numPasswordMeetingAllCriteria
  pure ()