module Propane.Transform
    ( scale, translate, rotate
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
