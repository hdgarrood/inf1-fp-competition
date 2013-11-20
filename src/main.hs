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
    writePng (nameFrom x) img)
    where
        nameFrom :: Int -> String
        nameFrom x = "output/frame" ++ (printf "%05d" x) ++ ".png"
        images = take 100 $ magnifiedRender
                                (640, 480)
                                wholeMandelbrot
                                target
                                (mandelbrot 100)
        target = 0.001643721971154 :+ 0.822467633298876
