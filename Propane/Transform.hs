-- | Transformations on images, animations, etc.
module Propane.Transform
    ( -- * Transforming scalars
      unbal, balun

      -- * Manipulating space
    , scale, scale2, translate, rotate

      -- * Manipulating time
    , speed, shift

      -- * Miscellaneous
    , spaced
    ) where

import Propane.Types

-- | Scale an @'Image'@ by the given factor.
scale :: R -> Image a -> Image a
scale s im (x,y) = im (x/s, y/s)

-- | Scale an @'Image'@ by the given factors
-- in /x/ and /y/ dimensions, respectively.
scale2 :: (R,R) -> Image a -> Image a
scale2 (sx,sy) im (x,y) = im (x/sx, y/sy)

-- | Translate an @'Image'@ by some displacement.
translate :: R2 -> Image a -> Image a
translate (dx,dy) im (x,y) = im (x-dx, y-dy)

-- | Rotate an @'Image'@ by some angle, in radians.
rotate :: Angle -> Image a -> Image a
rotate th im = \(x,y) -> im (x*cth + y*sth, y*cth + x*sth) where
    cth = cos (-th)
    sth = sin (-th)

-- | Multiply @'Animation'@ speed by the given factor.
speed :: Time -> Animation a -> Animation a
speed r ani t = ani (r*t)

-- | Shift an @'Animation'@ in time.  Positive offsets will
-- cause things to happen later.
shift :: Time -> Animation a -> Animation a
shift dt ani t = ani (t-dt)

-- | Squish the interval [-1, 1] into [0, 1].
unbal :: R -> R
unbal n = (n + 1) / 2

-- | Stretch the interval [0, 1] into [-1, 1].
balun :: R -> R
balun n = (n * 2) - 1

-- | @'spaced' n a b@ is a list of @n@ values, evenly spaced
-- from @a@ (included) to @b@ (not included), such that the next
-- evenly-spaced value would be @b@.
spaced :: (Fractional a) => Count -> a -> a -> [a]
spaced n a b = take n (iterate (+i) a) where
    i = (b - a) / fromIntegral n
