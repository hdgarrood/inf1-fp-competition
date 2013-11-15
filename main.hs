module Main where

import Graphics.UI.GLUT

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello, world!"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    flush


