module Magnifying where

import Data.Complex
import Codec.Picture

import Rendering
import Fractals
import Vec2

toVec2 :: C -> Vec2
toVec2 (a :+ b) = (a, b)

wholeMandelbrot :: PlotArea
wholeMandelbrot = ((-2, 1), (1, -1))

-- Take a PlotArea and a complex number, and return an infinite list of plot
-- areas, produced by zooming in on that number.
magnify :: Double -> C -> PlotArea -> [PlotArea]
magnify ratio (a :+ b) ((x1, y1), (x2, y2)) =
    nextArea : magnify ratio (a :+ b) nextArea
    where
        (x3, x4) = go x1 x2 a
        (y3, y4) = go y1 y2 b

        go a b c = (first, second)
            where
                l      = ratio * (b - a)
                first  = c - (l * (c - a) / (b - a))
                second = first + l

        nextArea = ((x3, y3), (x4, y4))

-- Similar arguments to 'render', except that this function also takes a Vec2
-- which specifies a point to zoom in on. Returns an infinite list.
magnifiedRender :: Double           -- magnification ratio
                -> ImageDimensions  -- dimensions of produced image
                -> PlotArea         -- initial plotting area
                -> C                -- a point in complex plane to zoom in on
                -> RenderFunction
                -> [Image PixelRGB8]
magnifiedRender ratio dims initialArea z f =
    map (\area -> render dims area f) areas
    where
        areas = magnify ratio z initialArea
