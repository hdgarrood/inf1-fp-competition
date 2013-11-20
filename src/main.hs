module Main where

import Data.Complex
import Codec.Picture
import Control.Monad
import Text.Printf
import Control.Concurrent
import Control.Parallel.Strategies
import System.Directory
import System.Exit

import Rendering
import Magnifying
import Fractals
import Vec2

main :: IO ()
main = do
    createOutputDir
    cleanOutputDir
    mapM_ writeToDisk (zip [0,1..] images)
    where
        outputDir = "output"

        nameFrom :: Int -> String
        nameFrom x = outputDir ++ "/frame" ++ (printf "%05d" x) ++ ".png"

        numFrames :: Int
        numFrames = 200

        target = (-0.743643884) :+ 0.131825909

        images :: [Image PixelRGB8]
        images = take numFrames $
            magnifiedRender
                (320, 240)
                wholeMandelbrot
                target
                (mandelbrot 2000)

        images' :: [Image PixelRGB8]
        images' = images `using` (parMap id)

        showProgress :: Int -> IO ()
        showProgress x = putStrLn $
            printf "Rendering (%d / %d) ...    " x numFrames

        writeToDisk :: (Int, Image PixelRGB8) -> IO ()
        writeToDisk (x, img) = writePng (nameFrom x) img

        createOutputDir = createDirectoryIfMissing True outputDir

        getDirectoryContents' =
            liftM (filter (not . (`elem` [".", ".."]))) .
            getDirectoryContents

        cleanOutputDir = do
            fs <- getDirectoryContents' outputDir
            let shouldAsk = length fs > 0

            ok <- if shouldAsk
                then confirm $ "delete " ++ show (length fs) ++
                          " files in ./output/ ?"
                else return True

            if ok
                then do
                    removeDirectoryRecursive outputDir
                    createOutputDir
                else do
                    putStrLn "doing nothing."
                    exitWith (ExitFailure 1)

        confirm :: String -> IO Bool
        confirm question = do
            putStrLn $ question ++ " [y/n]"
            ch <- getChar
            return $ case ch of
                'y' -> True
                'Y' -> True
                _   -> False
