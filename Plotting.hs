module Plotting (plot, PlotArea) where

import Graphics.UI.GLUT

import Vec2

type PlotArea = (Vec2, Vec2)

plot :: PlotArea -> (Vec2 -> Color3 GLfloat) -> IO ()
plot plotArea f = renderPrimitive Points $ mapM_ (\x -> do
        color $ f (windowToPlane plotArea x)
        vertexVec x)
        windowCoords
    where
        vertexVec (x,y) = vertex $ Vertex2 x y

windowToPlane :: PlotArea -> Vec2 -> Vec2
windowToPlane (topLeft, bottomRight) x =
    topLeft `vecAdd` (vecMult scale x')
    where
        scale = vecLength (bottomRight `vecSub` topLeft)
        x'    = normalise x

windowCoords :: [Vec2]
windowCoords = [ (x, y) | x <- values, y <- values ]
    where
        values = takeWhile (<= max) [min, min+step..]
        max    = 1
        min    = -1
        step   = 0.002
