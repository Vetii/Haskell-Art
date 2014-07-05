module Harmony where
-- Implementing rules of color harmony.

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Utils

-- A Hue is all colors with the same hue.
-- saturation and brightness not determined.
type Hue a = a -> a -> RGB a

-- A Nuance has hue and saturation determined.
type Nuance a = a -> RGB a

lightnesscmp :: (Ord a, Fractional a) => RGB a -> RGB a -> Ordering
lightnesscmp c1 c2 = compare (lightness c1) (lightness c2)

-- HUE
monochrome :: RealFrac a => a -> [Hue a]
monochrome hue = [hsl hue]

monochroms :: (RealFrac a) => [a] -> a -> [Hue a]
monochroms offsets hue = offsets >>= (\h -> monochrome (hue + h))

analogous :: (RealFrac a) => a -> [Hue a]
analogous hue = monochroms [(-36), 0, 36] hue 

complement :: (RealFrac a) => a -> [Hue a]
complement hue = monochroms [0, 180] hue 

splitComplement :: (RealFrac a) => a -> [Hue a]
splitComplement hue = monochroms [0, 288, 432] hue

tetratic :: (RealFrac a) => a -> [Hue a]
tetratic hue = monochroms [(-72), 72, 288, 432] hue

-- SATURATION
desaturate :: (RealFrac a) => [Hue a] -> [Nuance a]
desaturate shades = map (\s -> s 0.0)  shades

saturate :: (RealFrac a) => [Hue a] -> [Nuance a]
saturate shades = map (\s -> s 1.0) shades

midsaturate :: (RealFrac a) => [Hue a] -> [Nuance a]
midsaturate shades = map (\s -> s 0.5) shades

separateSat :: (RealFrac a) => a -> [Hue a] -> [Nuance a]
separateSat x shades = shades >>= (\s -> map s (separated x))

-- Value
dark :: (RealFrac a) => [Nuance a] -> [RGB a]
dark nuances = map (\s -> s 0.0) nuances

light :: (RealFrac a) => [Nuance a] -> [RGB a]
light nuances = map (\s -> s 1.0) nuances

separateLum :: (RealFrac a) => a -> [Nuance a] -> [RGB a]
separateLum x nuances = nuances >>= (\s -> map s (separated x))
