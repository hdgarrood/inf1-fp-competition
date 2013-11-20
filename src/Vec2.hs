module Vec2 where

-- top-right and bottom-left co-ordinates of the rectangular plot area
type Vec2 = (Double, Double)

vecAdd :: Vec2 -> Vec2 -> Vec2
vecAdd (a,b) (c,d) = (a+c, b+d)

vecSub :: Vec2 -> Vec2 -> Vec2
vecSub a b = a `vecAdd` (vecMult (-1) b)

-- scalar multiplication
vecMult :: Double -> Vec2 -> Vec2
vecMult k (a,b) = (k*a, k*b)

-- dot product
vecDot :: Vec2 -> Vec2 -> Double
vecDot (a,b) (c,d) = (a*c) + (b*d)

vecLength :: Vec2 -> Double
vecLength a = sqrt $ a `vecDot` a

-- turn a vector into a unit vector in the same direction
normalise :: Vec2 -> Vec2
normalise a = vecMult (1 / vecLength a) a
