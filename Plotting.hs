module Plotting (plot, PlotArea) where

import Graphics.UI.GLUT

import Vec2

-- A rectangle, represented as a top-left corner, and a bottom-right corner.
type Rectangle = (Vec2, Vec2)

-- Translate a vector from one rectangle to another
translateRectangle :: Rectangle -> Rectangle -> Vec2 -> Vec2
translateRectangle rect1 rect2 vec = (x', y')
    where
        ((r1x1, r1y1), (r1x2, r1y2)) = rect1
        ((r2x1, r2y1), (r2x2, r2y2)) = rect2

        (x, y) = vec
        x'     = r2x1 + ((x - r1x1) * (r2x2 - r2x1) / r1x2 - r1x1)
        y'     = r2y1 + ((y - r1y1) * (r2y2 - r2y1) / r1y2 - r1y1)

type PlotArea = Rectangle
plot :: PlotArea -> (Vec2 -> Color3 GLfloat) -> IO ()
plot plotArea f = renderPrimitive Points $ mapM_
        (\x -> do
            color (f' x)
            vertexVec x)
        windowCoords
    where
        vertexVec (x,y) = vertex $ Vertex2 x y
        f' = f . windowToPlane plotArea

windowRectangle :: Rectangle
windowRectangle = ((-1,1),(1,-1))

-- translate from the OpenGL window to some other plot area
windowToPlane :: PlotArea -> Vec2 -> Vec2
windowToPlane = translateRectangle windowRectangle

-- A list of all the points to render
windowCoords :: [Vec2]
windowCoords = [ (x, y) | x <- values, y <- values ]
    where
        values = takeWhile (<= max) [min, min+step..]
        max    = 1
        min    = -1
        step   = 0.002
