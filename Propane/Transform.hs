module Propane.Transform
    ( scale, translate, rotate
    , unbal, balun
    , spaced
    ) where

import Propane.Types

scale :: R -> Image a -> Image a
scale s im (x,y) = im (x/s, y/s)

translate :: R2 -> Image a -> Image a
translate (dx,dy) im (x,y) = im (x-dx, y-dy)

rotate :: Angle -> Image a -> Image a
rotate th im = \(x,y) -> im (x*cth + y*sth, y*cth + x*sth) where
    cth = cos (-th)
    sth = sin (-th)

unbal :: R -> R
unbal n = (n + 1) / 2

balun :: R -> R
balun n = (n * 2) - 1

spaced :: (Fractional a) => Count -> a -> a -> [a]
spaced n a b = take n (iterate (+i) a) where
    i = (b - a) / fromIntegral n
