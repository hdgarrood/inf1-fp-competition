Down the Rabbit Hole
====================

My entry to the Inf-1 FP competition, 2013.

Contents
--------

A literate Haskell source file, Mandelbrot.lhs, containing all my code (and
some prose!)

A .cabal file, enabling this to be built with cabal. It does have some
dependencies, so I would recommend using cabal. It should just be a case of:

    cabal configure
    cabal build
    ./dist/build/mandelbrot/mandelbrot +RTS -N4 # or however many cores you
                                                # want to use

A directory, 'output', which is where any generated images will go if you run
the program.

A directory, 'samples', which contains output from running the program. It also
contains an animated GIF image, produced by stitching them all together.
