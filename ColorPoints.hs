module ColorPoints where

import Data.Colour.RGBSpace
import Data.Colour
import Data.Vect.Double

data ColorPoint a = ColorPoint { color :: Colour a, pos :: Vec2 }

factor :: Vec2 -> Vec2 -> Double
factor pos p = min (1/(d^^2)) 1.0
    where d = distance pos p

getColor :: Vec2 -> [ColorPoint Double] -> Colour Double
getColor p points = foldl blend' (black) points
    where blend'  = (\acc x -> blend (factor p (pos x)) acc (color x)) 
