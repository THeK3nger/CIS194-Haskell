{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n = lastDigit n : (toRevDigits . dropLastDigit) n

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:ls) = x:(2*y):doubleEveryOther ls
doubleEveryOther (x:ls) = x:doubleEveryOther ls
doubleEveryOther [] = []


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x y -> y + lastDigit x + dropLastDigit x) 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn ccode = let magic_code = (sumDigits . doubleEveryOther . toRevDigits) ccode
  in (mod magic_code 10 == 0)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

-- | Move h discs from a to b with temp c
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi h a b c = (hanoi (h-1) a c b) ++ [(a,b)] ++ (hanoi (h-1) c b a)
