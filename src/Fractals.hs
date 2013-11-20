module Fractals where

import Data.Complex
import Data.Vector (Vector)
import qualified Data.Vector as V
import Codec.Picture

import Vec2

type C = Complex Double
type ColourPalette = Vector PixelRGB8

-- Given the intended length, make a colour palette.
makeColourPalette :: Int -> ColourPalette
makeColourPalette len = V.fromList $ take len $ map toColour [1,2..]
    where
        toColour x = PixelRGB8 (x `mod` 255) 255 255

defaultPalette :: ColourPalette
defaultPalette = makeColourPalette 500

toComplex :: Vec2 -> C
toComplex (a, b) = a :+ b

-- escape time algorithm
-- Given a list of iterative results, and a predicate which says whether we're
-- done, return either a Left (), signalling that no elements satisfied the
-- predicate, or a Right Int, giving the number of iterations before escape.
escapeTime :: (a -> Bool) -> [a] -> Either () Int
escapeTime p xs =
    case filter p' xs' of
        []        -> Left ()
        ((x,_):_) -> Right x
    where
        p'  = p . snd
        xs' = zip [0,1..] xs

-- Turn an escape time into a pixel, with the help of a colour palette 
colourise :: ColourPalette -> Either () Int -> PixelRGB8
colourise _  (Left ()) = PixelRGB8 0 0 0
colourise cp (Right x) = retrieve cp x

-- Retrieve a colour from a colour palette.
retrieve :: ColourPalette -> Int -> PixelRGB8
retrieve cp x = cp V.! x'
    where
        x' = x `mod` V.length cp

-- mandelbrot
mandelbrot :: Int -> Vec2 -> PixelRGB8
mandelbrot maxIters vec = mandelbrot' maxIters $ toComplex vec

mandelbrot' :: Int -> C -> PixelRGB8
mandelbrot' maxIters c = colour
    where
        f       = (\z -> z^2 + c)
        series  = iterate f 0
        escape  = escapeTime ((> 2) . magnitude) series
        palette = defaultPalette
        colour  = colourise palette escape
