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
-- ============================================================================
-- Number of frames to render
numFrames :: Int
numFrames = 100

-- The complex number to zoom in on
target :: C
target = (-0.743643884) :+ 0.131825909

-- The dimensions of the image to produce
imageDimensions :: ImageDimensions
imageDimensions = (320, 240)

-- Maximum Mandelbrot iterations
maxIterations :: Int
maxIterations = 2000

-- Value at which to stop iterating. If a complex number's magnitude does not
-- reach the square root of this value after the chosen maximum iterations, it
-- is treated as a member of the Mandelbrot set and coloured black.
bailoutRadius :: Double
bailoutRadius = 2 ** 16

-- the ratio of one iteration's width (and height) to the next. Should be
-- between 0 and 1 for zooming in; probably nearer to 1 than 0.
magnificationRatio :: Double
magnificationRatio = 0.8
-- ============================================================================

main :: IO ()
main = do
    dir <- getOutputDir
    prepareOutputDir dir
    putStrLn "Rendering..."
    mapM_ (writeToDisk dir) (zip [0,1..] images')
    where
        getOutputDir :: IO FilePath
        getOutputDir = fmap (formatTime defaultTimeLocale "output/%y%m%d-%H%M%S")
                        getCurrentTime

        images :: [Image PixelRGB8]
        images = take numFrames $
            magnifiedRender
                magnificationRatio
                imageDimensions
                wholeMandelbrot
                target
                (mandelbrot bailoutRadius maxIterations)

        -- Same as images, but evaluated in parallel
        images' :: [Image PixelRGB8]
        images' = images `using` (parList rdeepseq)

        writeToDisk :: FilePath -> (Int, Image PixelRGB8) -> IO ()
        writeToDisk outputDir (x, img) = do
            writePng (outputDir </> frameName x) img
            putStrLn $ printf "Rendered: frame %d" x
            where
                frameName x = "frame" ++ (printf "%05d" x) ++ ".png"

        prepareOutputDir :: FilePath -> IO ()
        prepareOutputDir = createDirectory
