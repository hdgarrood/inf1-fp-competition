module Fractals where

import Data.Complex
import Codec.Picture

import Vec2

type C = Complex Double

mandelbrot :: Int -> Vec2 -> PixelRGB8
mandelbrot maxIters vec =
    if isMandelbrot z maxIters
        then black
        else purple
    where
        z      = toComplex vec
        black  = PixelRGB8 0 0 0
        purple = PixelRGB8 200 25 255 

toComplex :: Vec2 -> C
toComplex (a, b) = a :+ b

-- Is a complex number in the Mandelbrot set?
isMandelbrot :: C -> Int -> Bool
isMandelbrot c maxIterations = all ((< 2) . magnitude) results
    where
        f x     = x^2 + c
        series  = iterate f 0
        results = take maxIterations series
