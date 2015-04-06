module Composition where

import Data.Vect
import Utils

ruleOfThirds :: Vec2 -> Vec2 -> [Vec2]
ruleOfThirds min max = [ Vec2 x y | x <- xs, y <- ys]
    where xs = map (\x -> lerp (_1 min) (_1 max) x) [0, 1/3, 2/3, 3/3]
          ys = map (\y -> lerp (_2 min) (_2 max) y) [0, 1/3, 2/3, 3/3]

toPolar :: (RealFloat a, Floating a) => a -> a -> (a,a)
toPolar x y = (x',y')
    where x'  = r * (cos phi)
          y'  = r * (sin phi) 
          r   = sqrt (x ^ 2 + y ^ 2)
          phi = atan2 y x

fromPolar :: (RealFloat a) => a -> a -> (a,a)
fromPolar r t = (x,y)
    where x = r * (cos t)
          y = r * (sin t)

poissonDistrib :: [Vec2]
poissonDistrib = undefined
