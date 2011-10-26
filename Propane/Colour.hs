module Propane.Colour
    ( BaseColour
    , opaque, transparent, withOpacity
    , blend, over, darken, dissolve
    , cRGB,  cHSV , cGray
    , cRGBA, cHSVA, cGrayA
    ) where

import qualified Data.Colour              as C
import qualified Data.Colour.SRGB         as C
import qualified Data.Colour.RGBSpace     as C
import qualified Data.Colour.RGBSpace.HSV as C

import Propane.Types

type BaseColour = C.Colour R

opaque :: BaseColour -> Colour
opaque = C.opaque

withOpacity :: BaseColour -> R -> Colour
withOpacity = C.withOpacity

transparent :: Colour
transparent = C.transparent

blend :: R -> Colour -> Colour -> Colour
blend = C.blend

over :: Colour -> Colour -> Colour
over = C.over

darken :: R -> Colour -> Colour
darken = C.darken

dissolve :: R -> Colour -> Colour
dissolve = C.darken

cRGB :: R -> R -> R -> Colour
cRGB r g b = cRGBA r g b 1.0

cHSV :: R -> R -> R -> Colour
cHSV h s v = cHSVA h s v 1.0

cGray :: R -> Colour
cGray g = cGrayA g 1.0

cRGBA :: R -> R -> R -> R -> Colour
cRGBA r g b a = withOpacity (C.sRGB r g b) a

cHSVA :: R -> R -> R -> R -> Colour
cHSVA h s v a = withOpacity (C.uncurryRGB C.sRGB (C.hsv h s v)) a

cGrayA :: R -> R -> Colour
cGrayA g a = cRGBA g g g a
