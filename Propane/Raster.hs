module Propane.Raster
    ( rasterize
    ) where

import qualified Data.Colour      as C
import qualified Data.Colour.SRGB as C
import qualified Data.Array.Repa  as R
import Data.Array.Repa ( Z(..), (:.)(..) )

import Propane.Types

type W8888 = (Word8, Word8, Word8, Word8)

rasterize :: Size -> Image Colour -> Raster
rasterize (Size w h) im = Raster . chans $ R.fromFunction dim f where
    f = quant . im . point

    dw = fromIntegral w - 1
    dh = fromIntegral h - 1
    dim = Z :. w :. h

    point :: R.DIM2 -> R2
    point (Z :. x :. y) = (adj dw x, adj dh y) where
        adj d n = (2 * fromIntegral n / d) - 1

    quant :: Colour -> W8888
    quant c = (r, g, b, a) where
        C.RGB r g b = C.toSRGB24 (c `C.over` C.black)
        a = round (C.alphaChannel c * 255.0)

    chans :: R.Array R.DIM2 W8888 -> R.Array R.DIM3 Word8
    chans arr = R.traverse arr (:. 4) chan where
        chan a (Z :. x :. y :. c) = ix c (a (Z :. x :. y))
        ix 0 (r,_,_,_) = r
        ix 1 (_,g,_,_) = g
        ix 2 (_,_,b,_) = b
        ix 3 (_,_,_,a) = a
        ix _ _ = error "Propane.Quantize: internal error (bad ix)"
