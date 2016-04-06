module Color where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import System.Random

data Palette a = Palette { hues :: [a], 
                           saturations :: [a],
                           luminances :: [a] } 

allHues :: (Enum a, Num a) => [a]
allHues = [0..360]

allSaturations :: (Enum a, Fractional a) => [a]
allSaturations = [0..1.0]

allLuminances :: (Enum a, Fractional a) => [a]
allLuminances = [0..1.0]

-- MODIFIERS
modHues :: ([a] -> [a]) -> Palette a -> Palette a
modHues f (Palette h s l) = Palette (f h) s l

modSats :: ([a] -> [a]) -> Palette a -> Palette a  
modSats f (Palette h s l) = Palette h (f s) l

modLums :: ([a] -> [a]) -> Palette a -> Palette a  
modLums f (Palette h s l) = Palette h s (f l)

-- COMPARATORS
lightnesscmp :: (Ord a, Fractional a) => RGB a -> RGB a -> Ordering
lightnesscmp c1 c2 = compare (lightness c1) (lightness c2)

-- GENERATORS
--
-- Divide hue space in x parts
hueDivBy :: (Fractional a, Enum a) => a -> [a]
hueDivBy x = let offset = 360 / x
             in [0,offset..360]

-- Divide saturation in x parts
satDivBy :: (Fractional a, Enum a) => a -> [a]
satDivBy x = let offset = 1.0 / x
             in [0,offset..1.0]
--
-- Divide lum in x parts 
lumDivBy :: (Fractional a, Enum a) => a -> [a]
lumDivBy x = let offset = 1.0 / x
             in [0,offset..1.0]

colorsDivBy :: (Enum a, Num a, Fractional a) => a -> Palette a
colorsDivBy x = Palette (hueDivBy x) (satDivBy x) (lumDivBy x)

-- HUE MODIFICATION
-- Warm & cold colors
warm :: (Fractional a, Ord a) => Palette a -> Palette a 
warm = modHues (filter (<= 100))

cold :: (Fractional a, Ord a) => Palette a -> Palette a
cold = modHues (filter (>= 100))

withHue :: (Num a) => a -> Palette a -> Palette a
withHue h = modHues (const [h])

complement :: (Num a) => a -> Palette a -> Palette a
complement h = modHues (const ([h + 180]))

analogous :: (Num a) => a -> Palette a -> Palette a 
analogous h = modHues (const (map (h+) [(-36),36]))

splitComplement :: (Num a) => a -> Palette a -> Palette a
splitComplement h = modHues (const (map (h+) [288,432]))

-- SATURATION MODIFICATION
-- Dull & vibrant 
dull :: (Fractional a, Ord a) => Palette a -> Palette a
dull = modSats (filter (<= 0.4))

vibrant :: (Fractional a, Ord a) => Palette a -> Palette a
vibrant = modSats (filter (>= 0.6))

-- LUMINANCE MODIFICATION
-- Light and dark
dark :: (Fractional a, Ord a) => Palette a -> Palette a
dark = modLums (filter (<= 0.4))

light :: (Fractional a, Ord a) => Palette a -> Palette a
light = modLums (filter (>= 0.6))

paletteFrom :: (RealFrac a) => Palette a -> [RGB a] 
paletteFrom p = [hsl h s l | h <- hues p, s <- saturations p, l <- luminances p, l > 0]

randomPaletteFrom :: (RandomGen g, RealFrac a) => g -> Palette a -> [RGB a]
randomPaletteFrom g p = concat selectedCols 
    where selectedCols = zipWith (\x c -> if c then [x] else []) (cycle (paletteFrom p)) (randCoins) 
          randCoins = randoms g :: [Bool]
