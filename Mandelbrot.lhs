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
> import Codec.Picture

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

> colourPalette :: ColourPalette
> colourPalette = makeColourPalette 1000

1000 colours ought to be enough. We now need a function to combine a colour
palette and an escape time to produce a colour. The function that follows is
called the 'Normalised Iteration Count' colouring algorithm, apparently. I
didn't have time to read up on it properly but it's on Wikipedia.

> colourise :: ColourPalette -> Either () (Int, C) -> PixelRGB8
> colourise _  (Left ())      = PixelRGB8 0 0 0
> colourise cp (Right (n, z)) = interpolateColour c1 c2 frac
>     where
>         zn = magnitude z
>         nu = log ((log zn) / (log 2)) / (log 2)
>         c1 = retrieve cp n
>         c2 = retrieve cp (n+1)
>         frac = (fromIntegral $ n + 1) - nu

We now have everything we need to make our mandelbrot function!

It will take the maximum iterations and the bailout radius as parameters and
return a function of type C -> PixelRGB; that is, it takes a complex number and
gives us a colour.

> mandelbrot :: Double -> Int -> C -> PixelRGB8
> mandelbrot bailoutRadius maxIters c = colour
> where
>     f       = (\z -> z^2 + c)
>     series  = iterate f 0
>     results = take maxIters series
>     escaped = escape ((> bailoutRadius) . magnitudesq) results
>     palette = defaultPalette
>     colour  = colourise palette escaped

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
