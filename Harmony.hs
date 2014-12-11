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

warms :: (RealFrac a, Enum a) => [Hue a]
warms = monochroms [0,(100/4)..100] 0

colds :: (RealFrac a, Enum a) => [Hue a]
colds = monochroms [100,(100 + 180/4)..360] 0

-- SATURATION
desaturate :: (RealFrac a) => [Hue a] -> [Nuance a]
desaturate hues = map (\s -> s 0.0) hues 

saturate :: (RealFrac a) => [Hue a] -> [Nuance a]
saturate hues = map (\s -> s 1.0) hues

midsaturate :: (RealFrac a) => [Hue a] -> [Nuance a]
midsaturate hues = map (\s -> s 0.5) hues

-- Creates a saturated & un-saturated color for each hue.
separateSat :: (RealFrac a) => a -> [Hue a] -> [Nuance a]
separateSat x hues = hues >>= (\s -> map s (separated x))

-- Saturate half hues and de-saturate half hues
halfSatScheme :: (RealFrac a, Enum a) => [Hue a] -> [Nuance a]
halfSatScheme hues = (firstHalf hues >>= (\s -> map s blands)) ++
        (lastHalf hues >>= (\s -> map s sats)) 
    where blands = [0.2]
          sats   = [0.8]

-- VALUE
darks :: (RealFrac a, Enum a) => [Nuance a] -> [RGB a]
darks nuances = nuances >>= (\s -> map s [0.0,0.1..0.4])

lights :: (RealFrac a, Enum a) => [Nuance a] -> [RGB a]
lights nuances = nuances >>= (\s -> map s [0.6,0.7..1.0])

-- Creates a light & dark color for each Nuance.
separateLum :: (RealFrac a) => a -> [Nuance a] -> [RGB a]
separateLum x nuances = nuances >>= (\s -> map s (separated x))

-- Brightens half colors and darkens others
halfLumScheme :: (RealFrac a, Enum a) => [Nuance a] -> [RGB a]
halfLumScheme nuances = (darks (firstHalf nuances)) ++ (lights (lastHalf nuances))

colorscheme :: (RealFrac a, Enum a) => [Hue a] -> [RGB a] 
colorscheme hues = halfLumScheme (halfSatScheme hues) 
