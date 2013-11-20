module Main where

import Data.Complex
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Codec.Picture
import Control.Monad
import Text.Printf
import Control.Concurrent
import Control.Parallel.Strategies
import System.FilePath
import System.Directory
import System.Exit

import Rendering
import Magnifying
import Fractals
import Vec2

-- Rendering options
numFrames :: Int
numFrames = 20

-- the complex number to zoom in on
target :: C
target = (-0.743643884) :+ 0.131825909

main :: IO ()
main = do
    dir <- getOutputDir
    prepareOutputDir dir
    mapM_ (writeToDisk dir) (zip [0,1..] images')
    where
        getOutputDir :: IO FilePath
        getOutputDir = fmap (formatTime defaultTimeLocale "output/%y%m%d-%H%M%S")
                        getCurrentTime

        frameName :: Int -> FilePath
        frameName x = "frame" ++ (printf "%05d" x) ++ ".png"

        images :: [Image PixelRGB8]
        images = take numFrames $
            magnifiedRender
                (320, 240)
                wholeMandelbrot
                target
                (mandelbrot 2000)

        -- Same as images, but evaluated in parallel
        images' :: [Image PixelRGB8]
        images' = images `using` (parList rdeepseq)

        showProgress :: Int -> IO ()
        showProgress x = putStrLn $
            printf "Rendering (%d / %d) ...    " x numFrames

        writeToDisk :: FilePath -> (Int, Image PixelRGB8) -> IO ()
        writeToDisk outputDir (x, img) = do
            writePng (outputDir </> frameName x) img

        prepareOutputDir :: FilePath -> IO ()
        prepareOutputDir = createDirectory

        confirm :: String -> IO Bool
        confirm question = do
            putStr $ question ++ " [y/n] "
            ch <- getChar
            return $ case ch of
                'y' -> True
                'Y' -> True
                _   -> False
