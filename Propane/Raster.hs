module Propane.Raster
    ( rasterize
    , rastimate
    ) where

import qualified Data.Sequence    as S
import qualified Data.Colour      as C
import qualified Data.Colour.SRGB as C
import qualified Data.Array.Repa  as R
import Data.Array.Repa ( Z(..), (:.)(..) )

import Propane.Types
import Propane.Transform ( spaced )

type W8888 = (Word8, Word8, Word8, Word8)

rasterize :: Size -> Image Colour -> Raster
rasterize (Size w h) im = Raster . chans $ R.fromFunction dim f where
    f = quant . im . point

    dw = fromIntegral w - 1
    dh = fromIntegral h - 1
    dim = Z :. w :. h

    point :: R.DIM2 -> R2
    point (Z :. y :. x) = (adj dw x, adj dh y) where
        adj d n = (2 * fromIntegral n / d) - 1

    quant :: Colour -> W8888
    quant c = (r, g, b, a) where
        C.RGB r g b = C.toSRGB24 (c `C.over` C.black)
        a = round (C.alphaChannel c * 255.0)

    chans :: R.Array R.DIM2 W8888 -> R.Array R.DIM3 Word8
    chans arr = R.traverse arr (:. 4) chan where
        chan a (Z :. y :. x :. c) = ix c (a (Z :. y :. x))
        ix 0 (r,_,_,_) = r
        ix 1 (_,g,_,_) = g
        ix 2 (_,_,b,_) = b
        ix 3 (_,_,_,a) = a
        ix _ _ = error "Propane.Quantize: internal error (bad ix)"

rastimate :: Count -> Size -> Animation Colour -> Rastimation
rastimate n sz ani = Rastimation (S.fromList frames) where
    frames = map (rasterize sz . ani) (spaced n 0 1)
