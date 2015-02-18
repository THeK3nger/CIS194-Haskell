{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P (z:zs) == P (y:ys)
      | z == y          = P zs == P ys
      | otherwise       = False
    P [] == P (y:ys)
      | y == 0          = P [] == P ys
      | otherwise       = False
    P (z:zs) == P []
      | z == 0          = P zs == P []
      | otherwise       = False
    P [] == P []        = True

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p)      = go (length p - 1) (reverse p)
      where
        go 0 (0:_)  = ""
        go 0 (y:_)  = show y
        go i (0:ys) = go (i-1) ys
        go i (y:ys) = show y ++ "x^" ++ show i ++ " + " ++  go (i-1) ys
        go _ _      = "Invalid"

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2)          = P (pluslist p1 p2)
  where
    pluslist (y:ys) (z:zs)  = (y+z):pluslist ys zs
    pluslist [] ps          = ps
    pluslist ps []          = ps

-- Exercise 5 -----------------------------------------

scalar :: Num a => a -> [a] -> [a]
scalar s (x:xs) = s*x : scalar s xs
scalar _ []     = []

times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) = sum $ map P (timesList p1 p2 0)
  where
    timesList ys (z:zs) i = (replicate i 0 ++ scalar z ys) : timesList ys zs (i + 1)
    timesList _ [] _ = []

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger i = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
