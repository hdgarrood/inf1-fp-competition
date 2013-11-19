module Mandelbrot where

import Data.Complex
import Graphics.UI.GLUT

import Vec2

mandelbrot :: Int -> Vec2 -> Color3 GLfloat
mandelbrot maxIters vec =
    if isMandelbrot z maxIters
        then black
        else purple
    where
        z      = toComplex vec
        black  = Color3 0 0 0
        purple = Color3 0.7 0.1 1

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
