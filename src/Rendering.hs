module Rendering where

import Data.Complex
import Codec.Picture
import qualified Data.Vector as V

import Fractals

-- A rectangle in the complex plane. Represented as a top-left corner, and a
-- bottom-right corner.
type Rectangle = (C, C)

-- Translate a vector from one rectangle to another, preserving:
--  * the ratio of the distance from the left-hand edge to the vector's x-value
--  * the ratio of the distance from the top edge to the vector's y-value
translateRectangle :: Rectangle -> Rectangle -> C -> C
translateRectangle rect1 rect2 (x :+ y) = x' :+ y'
    where
        (r1x1 :+ r1y1, r1x2 :+ r1y2) = rect1
        (r2x1 :+ r2y1, r2x2 :+ r2y2) = rect2

        x'     = r2x1 + ((x - r1x1) * (r2x2 - r2x1) / (r1x2 - r1x1))
        y'     = r2y1 + ((y - r1y1) * (r2y2 - r2y1) / (r1y2 - r1y1))

-- Type aliases for rendering
type PlotArea        = Rectangle
type ImageDimensions = (Int, Int)
type RenderFunction  = C -> PixelRGB8

imageDimensionstoRectangle :: ImageDimensions -> Rectangle
imageDimensionstoRectangle dims = (0 :+ 0, intTupleToC dims)

intTupleToC :: (Int, Int) -> C
intTupleToC (x, y) = fromIntegral x :+ fromIntegral y

-- Render an image
render :: ImageDimensions -> PlotArea -> RenderFunction -> Image PixelRGB8
render dims plotArea f = generateImage renderPixel width height
    where
        (width, height) = dims

        translate :: C -> C
        translate = imageToPlane dims plotArea

        renderPixel :: Int -> Int -> PixelRGB8
        renderPixel x y = f . translate . intTupleToC $ (x, y)

-- translate from image dimensions to a plot area
imageToPlane :: ImageDimensions -> PlotArea -> C -> C
imageToPlane dims = translateRectangle (imageDimensionstoRectangle dims)
