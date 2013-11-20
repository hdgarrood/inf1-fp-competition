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

justALittleBit :: C -> PlotArea
justALittleBit c = c `plusMinus` (4 ** (-3), 3 ** (-3))

plusMinus :: C -> Vec2 -> PlotArea
plusMinus c v = (c' `vecAdd` v, c' `vecSub` v)
    where
        c' = toVec2 c

magnifiedBit :: PlotArea
magnifiedBit = justALittleBit c
    where
        c = 0.7381919 :+ 0.2326958

-- Take a PlotArea and a complex number, and return an infinite list of plot
-- areas, produced by zooming in on that number.
magnify :: C -> PlotArea -> [PlotArea]
magnify (a :+ b) ((x1, y1), (x2, y2)) = nextArea : magnify (a :+ b) nextArea
    where
        (x3, x4) = go x1 x2 a
        (y3, y4) = go y1 y2 b

        go a b c = (first, second)
            where
                l      = ratio * (b - a)
                first  = c - (l * (c - a) / (b - a))
                second = first + l

        -- the ratio of one iteration's width (and height) to the next
        ratio = 0.80
        nextArea = ((x3, y3), (x4, y4))


-- Similar arguments to 'render', except that this function also takes a Vec2
-- which specifies a point to zoom in on. Returns an infinite list.
magnifiedRender :: ImageDimensions  -- dimensions of produced image
                -> PlotArea         -- initial plotting area
                -> C                -- a point in complex plane to zoom in on
                -> RenderFunction
                -> [Image PixelRGB8]
magnifiedRender dims initialArea z f = map (\area -> render dims area f) areas
    where
        areas = magnify z initialArea
