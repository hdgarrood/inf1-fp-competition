module Fractals where

import Data.Complex
import Data.Vector (Vector)
import qualified Data.Vector as V
import Codec.Picture

import Vec2

type C = Complex Double

-- Find the square of the magnitude of a complex number
-- so that we don't do any unnecessary square rooting
magnitudesq :: Num a => Complex a -> a
magnitudesq (a :+ b) = a*a + b*b

type ColourPalette = Vector PixelRGB8

-- Given the intended length, make a colour palette.
makeColourPalette :: Int -> ColourPalette
makeColourPalette len = V.fromList $ take len $ map toColour [1,2..]
    where
        -- There's no real reasoning behind this other than it seemed to work
        -- quite well after some trial and error.
        toColour x = PixelRGB8
            ((x^5 - 7*x^3) `mod` 255)
            ((x^2 - 6*x)   `mod` 255)
            ((x^3 - 2*x^2) `mod` 255)

defaultPalette :: ColourPalette
defaultPalette = makeColourPalette 1000

toComplex :: Vec2 -> C
toComplex (a, b) = a :+ b


-- Given a list of iterative results, and a predicate which says whether we're
-- done, return either a Left (), signalling that no elements satisfied the
-- predicate, or a Right (Int, a), giving a) the number of iterations before
-- escape, and b) the first value to satisfy the predicate.
escape :: (a -> Bool) -> [a] -> Either () (Int, a)
escape p xs =
    case filter p' xs' of
        []    -> Left ()
        (x:_) -> Right x
    where
        p'  = p . snd
        xs' = zip [0,1..] xs

black :: PixelRGB8
black = PixelRGB8 0 0 0

-- Normalised iteration count colouring algorithm
-- Turn an escape time into a pixel, with the help of a colour palette
colourise :: ColourPalette -> Either () (Int, C) -> PixelRGB8
colourise _  (Left ())      = black
colourise cp (Right (n, z)) = interpolateColour c1 c2 frac
    where
        zn = magnitude z
        nu = log ((log zn) / (log 2)) / (log 2)
        c1 = retrieve cp n
        c2 = retrieve cp (n+1)
        frac = (fromIntegral $ n + 1) - nu

-- Interpolates between two colours, treating R, G, B separately.
interpolateColour :: RealFrac a => PixelRGB8 -> PixelRGB8 -> a -> PixelRGB8
interpolateColour (PixelRGB8 r1 g1 b1)
                  (PixelRGB8 r2 g2 b2)
                  frac =
    PixelRGB8 r g b
    where
        li x y = floor . linearInterpolate (fromIntegral x) (fromIntegral y)
        r = li r1 r2 frac
        g = li g1 g2 frac
        b = li b1 b2 frac

-- Find a value which is a proportion of the distance between x1 and x2, which
-- is equal to frac.
linearInterpolate :: RealFrac a => a -> a -> a -> a
linearInterpolate x1 x2 frac = x1 + (frac * (x2 - x1))

-- Retrieve a colour from a colour palette.
retrieve :: ColourPalette -> Int -> PixelRGB8
retrieve cp x = cp V.! x'
    where
        x' = x `mod` V.length cp

-- mandelbrot
mandelbrot :: Int -> Vec2 -> PixelRGB8
mandelbrot maxIters vec = mandelbrot' bailoutRadius maxIters $ toComplex vec
    where
        bailoutRadius = 2 ** 16

mandelbrot' :: Double -> Int -> C -> PixelRGB8
mandelbrot' bailoutRadius maxIters c = colour
    where
        f       = (\z -> z^2 + c)
        series  = iterate f 0
        results = take maxIters series
        escaped = escape ((> bailoutRadius) . magnitudesq) results
        palette = defaultPalette
        colour  = colourise palette escaped
