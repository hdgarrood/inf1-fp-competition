module Main where

import Data.Complex
import Codec.Picture
import Control.Monad
import Text.Printf

import Rendering
import Magnifying
import Fractals
import Vec2

main :: IO ()
main = forM_ (zip [0,1..] images) $ (\(x, img) -> do
    writePng (nameFrom x) img
    showProgress x)
    where
        nameFrom :: Int -> String
        nameFrom x = "output/frame" ++ (printf "%05d" x) ++ ".png"

        images = take numFrames $ magnifiedRender
                                    (320, 240)
                                    wholeMandelbrot
                                    target
                                    (mandelbrot 1000)
        target = (-0.7321919) :+ 0.2326958
        numFrames = 100

        showProgress :: Int -> IO ()
        showProgress x = putStrLn $
            printf "Rendering (%d / %d) ...    " x numFrames
