module Main where

import Graphics.UI.GLUT

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
    plot ((0,0), (255,255)) testPlotFunc
    flush

-- top-right and bottom-left co-ordinates of the rectangular plot area
type Vec2 = (GLfloat, GLfloat)

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (a,b) (c,d) = (a+c, b+d)

vecSub :: Vec2 -> Vec2 -> Vec2
vecSub a b = a `vecAdd` (vecMult (-1) b)

-- scalar multiplication.
vecMult :: GLfloat -> Vec2 -> Vec2
vecMult k (a,b) = (k*a, k*b)

vecDot :: Vec2 -> Vec2 -> GLfloat
vecDot (a,b) (c,d) = (a*c) + (b*d)

vecLength :: Vec2 -> GLfloat
vecLength a = sqrt $ a `vecDot` a

-- turn a vector into a unit vector in the same direction
normalise :: Vec2 -> Vec2
normalise a = vecMult (1 / vecLength a) a

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
testPlotFunc vec = Color3 a' b' 0
    where
        -- scale to length 0.5
        (a,  b)  = 0.5 `vecMult` (normalise vec)
        (a', b') = (a, b) `vecAdd` (0.5, 0.5)
