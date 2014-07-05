import Harmony
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Composition
import Data.Vect.Double
import Graphics.Rendering.Cairo
import Data.List
import System.Random

-- SETUP
canvasWidth :: Double
canvasWidth = 1366

canvasHeight :: Double
canvasHeight = 768

surface :: (Surface -> IO a) -> IO a
surface function = withSVGSurface "out.svg" canvasWidth canvasHeight function

sourceFromRGB :: RGB Double -> Render ()
sourceFromRGB colour = uncurryRGB (setSourceRGB) colour

-- DRAW
mkLine :: Vec2 -- Start position
    -> Double -- Length of line
    -> Double -- Thickness
    -> RGB Double -- Color
    -> Render()
mkLine p l t c = do
    sourceFromRGB c
    setLineWidth t 
    moveTo (_1 pp) (_2 pp)
    lineTo (_1 np) (_2 np)
    stroke
    where np = p &+ (Vec2 l l)
          pp = p &- (Vec2 l l)

background :: RGB Double -> Render ()
background color = do
    sourceFromRGB color
    rectangle 0 0 canvasWidth canvasHeight 
    fill

drawing :: [Double] -> [RGB Double] -> Render ()
drawing [] _ = return ()
drawing _ [] = return ()
drawing (r1:r2:rands) (c:colors) = do
    let xmax = canvasWidth 
    let ymax = canvasHeight 
    let pos = Vec2 (r1 * xmax) (r2 * ymax)
    let line = 50
    let lineWidth = 50 * (1 - (lightness c))
    mkLine pos line lineWidth c
    drawing rands colors 

-- MAIN
main = do
    gen <- getStdGen
    let rands = randomRs (0.0, 1.0) gen :: [Double] 
    let palette = concat [ separateLum r (separateSat 0.8 (monochrome 20)) | r <- rands ]
    let palette'= sortBy (lightnesscmp) $ take 1000 (concat (repeat (palette)))
    -- Actual drawing.
    surface (flip renderWith $ (drawing rands palette'))
