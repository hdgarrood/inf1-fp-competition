module Main where

import Codec.Picture

import Rendering
import Fractals

main :: IO ()
main = writePng "out.png" $
    render (320, 240) ((-2, 1), (1, -1)) (mandelbrot 100)
