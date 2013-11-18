module Main where

import Graphics.UI.GLUT
import Data.Complex

import Vec2

windowWidth  = 640
windowHeight = 480

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello, world!"
    displayCallback $= display
    windowSize      $= Size windowWidth windowHeight
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    plot ((-2,1), (1,-1)) testPlotFunc
    flush

type PlotArea = (Vec2, Vec2)

plot :: PlotArea -> (Vec2 -> Color3 GLfloat) -> IO ()
plot plotArea f = renderPrimitive Points $ mapM_ (\x -> do
        color $ f (windowToPlane x)
        vertexVec x) windowCoords
    where
        vertexVec :: Vec2 -> IO ()
        vertexVec (x,y) = vertex $ Vertex2 x y

        (topLeft, bottomRight) = plotArea

        windowToPlane :: Vec2 -> Vec2
        windowToPlane x = topLeft `vecAdd` (vecMult scale x')
            where
                scale = vecLength (bottomRight `vecSub` topLeft)
                x'    = normalise x

windowCoords :: [Vec2]
windowCoords = [ (x, y) | x <- values, y <- values ]
    where values = takeWhile (<= max) [min, min+step..]
          max    = 1
          min    = -1
          step   = 0.002

testPlotFunc :: Vec2 -> Color3 GLfloat
testPlotFunc vec =
    if isMandelbrot z iters
        then black
        else purple
    where
        z      = toComplex vec
        black  = Color3 0 0 0
        purple = Color3 0.7 0.1 1
        iters  = 3

toComplex :: Vec2 -> C
toComplex (a, b) = a :+ b

type C = Complex GLfloat

-- Is a complex number in the Mandelbrot set?
isMandelbrot :: C -> Int -> Bool
isMandelbrot c maxIterations = all ((< 2) . magnitude) results
    where
        f x     = x^2 + c
        series  = iterate f 0
        results = take maxIterations series
