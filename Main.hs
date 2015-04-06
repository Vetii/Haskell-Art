import Composition
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.List
import Data.Vect.Double
import Graphics.Rendering.Cairo
import Color
import Spirograph
import System.Random
import Utils

-- SETUP
canvasWidth :: Double
canvasWidth = 1366

canvasHeight :: Double
canvasHeight = 768

surface :: (Surface -> IO a) -> String -> IO a
surface function filename = withSVGSurface filename canvasWidth canvasHeight function

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

spiro :: [Int] -- Fixed wheels
    -> [Int] -- Moving wheels
    -> [Double] -- ratios (l)
    -> [RGB Double] -- colours
    -> Render () -- rendering.
spiro fixed moving ratios colours = do
    translate (canvasWidth / 2) (canvasHeight / 2)
    let spires = zipWith3 spirograph fixed moving ratios
    sequence_ (zipWith mkPath (map (id) spires) (tail colours))

mkPath :: [(Double, Double)] -- list of points
    -> RGB Double -- colours
    -> Render() -- rendered
mkPath points colour = do
    newPath
    sourceFromRGB colour 
    setLineJoin LineJoinRound
    setLineWidth 5 -- thin line
    sequence_ (map (\(x,y) -> lineTo x y) points)
    closePath
    stroke

dots :: Spirograph Double
    -> RGB Double
    -> Render()
dots spir colour = sequence_ (map draw spir)
    where draw (x,y) = do 
                        save 
                        translate x y
                        sourceFromRGB colour
                        arc 0 0 1 0 (2 * pi)
                        fill 
                        restore 

drawSpiro :: Int -> IO()
drawSpiro param = do
    gen <- getStdGen 
    let list    = randomRs (24,400) gen
    let sizes   = mineSizes param list (tail list) 
    let fixed   = repeat (head (map (fst) sizes)) >>= (\x -> [x * i | i <- [1,2,2,1]])
    let moved   = repeat (head (map (snd) sizes)) >>= (\x -> [x * i | i <- [1,2,1,2]])
    let ratios  = randomRs (0,1) gen >>= (\x -> [x,(1 - x)]) :: [Double]
    let hue     = fst $ randomR (0, 360) gen
    let fg = (randomPaletteFrom gen) $ (light . (withHue hue) . vibrant) $ colorsDivBy 6
    let bg = (randomPaletteFrom gen) $ (dark  . (complement hue) . dull) $ colorsDivBy 6
    let palette = sortBy (lightnesscmp) $ (take 10 fg) ++ (take 10 bg)
    surface (flip renderWith $ (background (head palette)) >> (spiro fixed moved ratios (tail palette))) ("out"++(show param)++".svg")

-- MAIN
main = do
    sequence_ $ map (drawSpiro) [2..9]
