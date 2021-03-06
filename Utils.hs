module Utils where

import System.Random

-- For the pattern 80% - 20%; etc.
separated :: (RealFrac a) => a -> [a]
separated x = [fx, 1.0 - fx]
    where fx = snd (properFraction x)  -- 2.3 -> 0.3

-- How to cut a list in two parts.
-- (Useful for separation of foreground / background
firstHalf :: [a] -> [a]
firstHalf list = take (div (length list) 2) list

lastHalf :: [a] -> [a]
lastHalf list  = drop (div (length list) 2) list

shuffle :: (RandomGen g) => g -> [a] -> [a]
shuffle g list = concat $ zipWith3 (\x y c -> if c then [x,y] else [y,x]) list (shuffle g (tail list)) (randoms g :: [Bool])

lerp :: (Fractional a, Ord a) => a -> a -> a -> a
lerp a b factor 
    | factor >= 1.0 = b
    | otherwise = a + ((b - a) * factor) 

-- Useful for the golden ratio
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
