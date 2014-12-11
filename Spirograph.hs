module Spirograph where

import Data.Ratio
import System.Random

type Point a = (a, a) 
type Spirograph a = [Point a]

newPoint :: (Floating a) 
    => a -- scaling parameter R
    -> a -- ratio of size of first and second wheel (0 <= k <= 1)
    -> a -- ratio p / r (p being where the pen is on the wheel and r the radius.
    -> a -- t the angle 0 <= t <= 2 * pi.
    -> Point a -- coords of x and y.
newPoint r k l t = (x t,y t)
    where x = \t -> r * ((1-k) * cos t + l * k * cos (((1-k)/k) * t))
          y = \t -> r * ((1-k) * sin t - l * k * sin (((1-k)/k) * t))

spirograph :: (Integral a, Floating b, Ord b, Enum b) => a -- Size of first wheel
    -> a -- Size of second wheel 
    -> b -- l: ratio p / r (0 <= l <= 1)
    -> Spirograph b
spirograph rfixed rmoving l = map (newPoint (fromIntegral rfixed) k l) [0.0,0.05..s] 
    where k = (fromIntegral rmoving) / (fromIntegral rfixed)
          n = (lcm rfixed rmoving) `quot` rfixed -- number of rows to complete
          s = 2 * pi * (fromIntegral n) 

rescale :: (Fractional a) => a -> Spirograph a -> Spirograph a 
rescale f points = map (\(x, y) -> (x * f, y * f)) points

-- To search for size of spirographs that return pretty results.
mineSizes :: (RandomGen g, Random a, Integral a) => g -> a -> (a,a) -> (a,a) -> [(a,a)] 
mineSizes gen n fixBounds mobileBounds = filter (\x -> t x == n) z
    where t = \(x,y) -> (lcm x y) `quot` x 
          z = if fixBounds == mobileBounds then zip f (tail f) else zip f m 
          f = randomRs fixBounds gen
          m = randomRs mobileBounds gen

area :: (Num a, Ord a) => Spirograph a -> a
area s = deltax * deltay 
    where deltax = (foldr (max . fst) 0 s) - (foldr (min . fst) 10000 s)
          deltay = (foldr (max . snd) 0 s) - (foldr (min . snd) 10000 s)
