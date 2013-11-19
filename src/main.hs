module Main where

import Graphics.UI.GLUT
import Data.Complex

import Vec2
import Plotting
import Mandelbrot

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello, world!"
    displayCallback $= display
    windowSize      $= Size windowWidth windowHeight
    mainLoop

maxIterations = 100

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    plot ((-2,1), (1,-1)) (mandelbrot maxIterations)
    flush

