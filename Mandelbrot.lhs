Down the Rabbit Hole
====================

We will be creating a sequence of images of the Mandelbrot set, where the
camera zooms in on a particular point, and the intricate detail and repeating
patterns slowly become visible.

If you're not familiar with the Mandelbrot set, it is the set of all complex
numbers 'c' for which its series never gives a number with a magnitude greater
than 2, where the series is given by z(n+1) = z(n) ^ 2 + c. That is, to get the
next term, square the last one, and add c (the number you started with).

On to the code.

> import Data.Complex
> import Data.Vector (Vector)
> import qualified Data.Vector as V
> import Data.Time.Clock
> import Data.Time.Format
> import Codec.Picture
> import Control.Monad
> import Control.Concurrent
> import Control.Parallel.Strategies
> import Control.DeepSeq
> import System.FilePath
> import System.Directory
> import System.Locale
> import System.Exit
> import Text.Printf

The first decision to make is the type we're using for complex numbers.
Luckily, the Data.Complex module does most of the work for us, but it's up to
us what types are used to represent the real and imaginary parts. I'm going to
use Doubles, as well as a type synonym for convenience.

> type C = Complex Double

We can make our image prettier by colouring values which fall outside the set,
based on how quickly their series diverge. To do this, we'll choose some very
large bailout radius, and colour the image based on how many iterations of the
series it took to reach that value. If the series doesn't reach that value for
a given maximum number of iterations, we assume it's in the set, and colour it
black.

> maxIterations :: Int
> maxIterations = 2000
>
> bailoutRadius :: Double
> bailoutRadius = 2 ** 16

Before we can implement our mandelbrot function, we should implement this
escape-time algorithm separately.

> escape :: (a -> Bool) -> [a] -> Either () (Int, a)
> escape p xs =
>     case filter p' xs' of
>         []    -> Left ()
>         (x:_) -> Right x
>     where
>         p'  = p . snd
>         xs' = zip [0,1..] xs

Given a list of iterative results, and a predicate which says whether we're
done, this function gives us either a Left (), signalling that no elements
satisfied the predicate, or a Right (Int, a), giving a) the number of
iterations before 'escape', and b) the first value to satisfy the predicate.
This should do nicely.

We now also need some way of converting an integer representing how many steps
until escape occurred into a colour.

> type ColourPalette = Vector PixelRGB8

I've used a Vector (rather than, say, a list) so that we get O(1) indexing.
Hooray!

> makeColourPalette :: Int -> ColourPalette
> makeColourPalette len = V.fromList $ take len $ map toColour [1,2..]
>     where
>         toColour x = PixelRGB8
>             ((x^5 - 7*x^3) `mod` 255)
>             ((x^2 - 6*x)   `mod` 255)
>             ((x^3 - 2*x^2) `mod` 255)

I chose these polynomials so that two adjacent colours in the palette will not,
in most cases, look similar. The chaos of the colours goes well with the chaos
of the Mandelbrot set which they're depicting.

> palette :: ColourPalette
> palette = makeColourPalette 1000

1000 colours ought to be enough. However, what if the maximum number of
iterations is larger than this, and we try to take more colours than there are
in our colour palette? Let's define a new function to alleviate this. If we
just take the value modulus the number of colours we have, we'll always be ok.

> retrieve :: ColourPalette -> Int -> PixelRGB8
> retrieve cp x = cp V.! x'
>     where
>         x' = x `mod` V.length cp

We now need a function to combine a colour palette and an escape time to
produce a colour. The function that follows is called the 'Normalised Iteration
Count' colouring algorithm, apparently. I didn't have time to read up on it
properly but it's on Wikipedia.

> colourise :: ColourPalette -> Either () (Int, C) -> PixelRGB8
> colourise _  (Left ())      = PixelRGB8 0 0 0
> colourise cp (Right (n, z)) = interpolateColour c1 c2 frac
>     where
>         zn = magnitude z
>         nu = log ((log zn) / (log 2)) / (log 2)
>         c1 = retrieve cp n
>         c2 = retrieve cp (n+1)
>         frac = (fromIntegral $ n + 1) - nu

This function creates a colour by interpolating between two colours in the
palette. The interpolateColour function is defined below. It uses another
function, linearInterpolate, which just returns a value which is between x1 and
x2, where 'frac' is the distance along the line between them.

> linearInterpolate :: RealFrac a => a -> a -> a -> a
> linearInterpolate x1 x2 frac = x1 + (frac * (x2 - x1))
>
> interpolateColour :: RealFrac a => PixelRGB8 -> PixelRGB8 -> a -> PixelRGB8
> interpolateColour (PixelRGB8 r1 g1 b1)
>                   (PixelRGB8 r2 g2 b2)
>                   frac =
>     PixelRGB8 r g b
>     where
>         li x y = floor . linearInterpolate (fromIntegral x) (fromIntegral y)
>         r = li r1 r2 frac
>         g = li g1 g2 frac
>         b = li b1 b2 frac

We now have everything we need to make our mandelbrot function!

It will take the maximum iterations and the bailout radius as parameters and
return a function of type C -> PixelRGB; that is, it takes a complex number and
gives us a colour.

> mandelbrot :: Double -> Int -> C -> PixelRGB8
> mandelbrot bailoutRadius maxIters c = colour
>     where
>         f       = (\z -> z^2 + c)
>         series  = iterate f 0
>         results = take maxIters series
>         escaped = escape ((> bailoutRadius) . magnitudesq) results
>         colour  = colourise palette escaped

What's that function, magnitudesq? The idea is that it's easier to compute the
square of the magnitude that the magnitude itself; we don't want to do any
unnecessary square rooting.

> magnitudesq :: Num a => Complex a -> a
> magnitudesq (a :+ b) = a*a + b*b

Now we need to think about how to generate the image. For this, let's have some
more type synonyms. A type representing a rectangle in the complex plane will
help:

> type Rectangle = (C, C)

These are just to make function signatures easier to read.

> type PlotArea        = Rectangle
> type ImageDimensions = (Int, Int)
> type RenderFunction  = C -> PixelRGB8

We need to be able to map a rectangle in the complex plane onto an image with a
given height and width. This function takes two rectangles and returns a
function C -> C which translates from one rectangle to the other using linear
interpolation.

> translateRectangle :: Rectangle -> Rectangle -> C -> C
> translateRectangle rect1 rect2 (x :+ y) = x' :+ y'
>     where
>         (r1x1 :+ r1y1, r1x2 :+ r1y2) = rect1
>         (r2x1 :+ r2y1, r2x2 :+ r2y2) = rect2
> 
>         x'     = r2x1 + ((x - r1x1) * (r2x2 - r2x1) / (r1x2 - r1x1))
>         y'     = r2y1 + ((y - r1y1) * (r2y2 - r2y1) / (r1y2 - r1y1))

We'll also need these:

> intTupleToC :: (Int, Int) -> C
> intTupleToC (x, y) = fromIntegral x :+ fromIntegral y
>
> imageDimensionstoRectangle :: ImageDimensions -> Rectangle
> imageDimensionstoRectangle dims = (0 :+ 0, intTupleToC dims)
>
> imageToPlane :: ImageDimensions -> PlotArea -> C -> C
> imageToPlane dims = translateRectangle (imageDimensionstoRectangle dims)

That's everything; we're ready for the main render function. This should take
the dimensions of the image we want, an area of the complex plane to plot, a
function which gives us a colour for each complex number, and return an image.
The JuicyPixels library gives us a function called generateImage for this
purpose.

> render :: ImageDimensions -> PlotArea -> RenderFunction -> Image PixelRGB8
> render dims plotArea f = generateImage renderPixel width height
>     where
>         (width, height) = dims
> 
>         translate :: C -> C
>         translate = imageToPlane dims plotArea
> 
>         renderPixel :: Int -> Int -> PixelRGB8
>         renderPixel x y = f . translate . intTupleToC $ (x, y)

Now we need to implement the magnification. We'll start with a function
'magnify,' which takes a magnification ratio, a complex number to zoom in on,
and a starting plot area, and returns an infinite list of smaller and smaller
rectangles, such that if we look at them all in order, it will appear that
we're zooming in towards the given number.

> magnify :: Double -> C -> PlotArea -> [PlotArea]
> magnify ratio (a :+ b) (x1 :+ y1, x2 :+ y2) =
>     nextArea : magnify ratio (a :+ b) nextArea
>     where
>         x3 :+ x4 = go x1 x2 a
>         y3 :+ y4 = go y1 y2 b
> 
>         go a b c = first :+ second
>             where
>                 l      = ratio * (b - a)
>                 first  = c - (l * (c - a) / (b - a))
>                 second = first + l
> 
>         nextArea = (x3 :+ y3, x4 :+ y4)

We use linear interpolation again at each stage to create a new rectangle,
which should have a width equal to the width of the previous rectangle
multiplied by the magnification ratio.

Given this, it's not difficult to implement a magnifying version of 'render'
which returns a list of images instead of just a single one.

> magnifiedRender :: Double           -- magnification ratio
>                 -> ImageDimensions  -- dimensions of produced image
>                 -> PlotArea         -- initial plotting area
>                 -> C                -- a point in complex plane to zoom in on
>                 -> RenderFunction
>                 -> [Image PixelRGB8]
> magnifiedRender ratio dims initialArea z f =
>     map (\area -> render dims area f) areas
>     where
>         areas = magnify ratio z initialArea

We're nearly there! There are a few more values we need to set first, though.

How many frames should we render?

> numFrames :: Int
> numFrames = 3

What number should we zoom in on?

> target :: C
> target = (-0.743643884) :+ 0.131825909

How large should our image be?

> imageDimensions :: ImageDimensions
> imageDimensions = (320, 240)

How quickly should we zoom in?

> magnificationRatio :: Double
> magnificationRatio = 0.8

What should we start off looking at? (This is an easy one; the whole set,
obviously!)

> wholeMandelbrot :: PlotArea
> wholeMandelbrot = ((-2) :+ 1.5, 1 :+ (-1.5))

That's everything we need to create our list of images.

> images :: [Image PixelRGB8]
> images = take numFrames $
>     magnifiedRender
>         magnificationRatio
>         imageDimensions
>         wholeMandelbrot
>         target
>         (mandelbrot bailoutRadius maxIterations)

"But wait!" I hear you cry. "Isn't Haskell supposed to be really good at
parallelizing computations?" Well, yeah. Don't worry -- we'll be using as many
cores as we have available. We just need a function to evaluate all the
elements of a list in parallel.

> parRnfList :: NFData a => Strategy [a]
> parRnfList []     = return []
> parRnfList (x:xs) = do
>     x'  <- rpar (force x)
>     xs' <- parRnfList xs
>     return $ x' : xs'

And now, we tell Haskell that this is how we'd like our images to be evaluated:

> images' :: [Image PixelRGB8]
> images' = images `using` parRnfList

(Isn't it awesome that we've parallelized all that by just adding an extra 7
lines to our previously sequential code?)

Now we just need a function to write an image to a file on the disk. Note that
this function should take an integer as well as an image, so that we know where
in the sequence it comes. JuicyPixels takes care of most of this for us, with
its 'writePng' function.

> writeToDisk :: FilePath -> (Int, Image PixelRGB8) -> IO ()
> writeToDisk outputDir (x, img) = do
>     writePng (outputDir </> frameName x) img
>     putStrLn $ printf "Rendered: frame %d" x
>     where
>         frameName x = "frame" ++ (printf "%05d" x) ++ ".png"

Finally, we implement main.

> main :: IO ()
> main = do
>     dir <- getOutputDir
>     createDirectory dir
>     mapM_ (writeToDisk dir) (zip [0,1..] images')
>     where
>         getOutputDir :: IO FilePath
>         getOutputDir =
>             fmap (formatTime defaultTimeLocale "output/%y%m%d-%H%M%S")
>                 getCurrentTime
> 
