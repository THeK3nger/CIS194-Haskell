{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Control.Monad

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches (x:xs) (y:ys)
  | x == y = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys
exactMatches _ _ = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code =
  let counter code_ c = (length . filter (== c)) code_
  in
    map (counter code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 =
  let internalMatch (x:xs) (y:ys) = min x y + internalMatch xs ys
      internalMatch _ _           = 0
  in
    internalMatch  (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess =
  let exacts    = exactMatches secret guess
      nonexacts = matches secret guess - exacts
  in
    Move guess exacts nonexacts

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e ne) code
  | newE == e && newNE == ne  = True
  | otherwise                 = False
  where
    (Move _ newE newNE) = getMove code guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n = replicateM n colors

-- Exercise 7 -----------------------------------------

solvePlus :: Code -> [Code] -> [Move]
solvePlus _ [] = []
solvePlus secret (x:xs)
  | e == length secret  = [currentmove]
  | otherwise           = currentmove : solvePlus secret (filterCodes currentmove xs)
  where
    currentmove = getMove secret x
    Move _ e ne = currentmove

solve :: Code -> [Move]
solve secret =
  let size = length secret
      allcodes = allCodes size
  in
      solvePlus secret allcodes

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
