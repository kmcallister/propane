-- | Constructing and manipulating colours.
--
-- This module exports a subset of the @colour@ package at simplified,
-- restricted types.  You can also use the full @colour@ package with
-- these values.
module Propane.Colour
    ( -- * Constructing opaque colours
      cRGB,  cHSV , cGray

      -- * Constructing colours with transparency
    , cRGBA, cHSVA, cGrayA

      -- * Constructing colours from names
    , BaseColour
    , opaque, withOpacity
    , transparent

      -- * Manipulating colours
    , blend, over, darken, dissolve

    ) where

import qualified Data.Colour              as C
import qualified Data.Colour.SRGB         as C
import qualified Data.Colour.RGBSpace     as C
import qualified Data.Colour.RGBSpace.HSV as C

import Propane.Types

-- | Represents a colour without alpha channel.
--
-- The module @Data.Colour.Names@ defines many colours of this
-- type.
--
-- The right-hand side refers to the type @'C.Colour'@ defined
-- in the @colour@ package.  Every other instance of @'Colour'@
-- in this page refers to the type defined in "Propane.Types".
--
-- This would be less confusing if Haddock displayed qualified
-- names, but at least the hyperlinks go to the right place.
type BaseColour = C.Colour R

-- | Creates an opaque @'Colour'@ from a @'BaseColour'@.
opaque :: BaseColour -> Colour
opaque = C.opaque

-- | Creates a @'Colour'@ from a @'BaseColour'@ with a given opacity.
withOpacity :: BaseColour -> R -> Colour
withOpacity = C.withOpacity

-- | A fully-transparent @'Colour'@.
transparent :: Colour
transparent = C.transparent

-- | Compute the weighted average of two @'Colour'@s. e.g.
--
-- >blend 0.4 a b = 0.4*a + 0.6*b
blend :: R -> Colour -> Colour -> Colour
blend = C.blend

-- | @c1 \`over\` c2@ returns the @'Colour'@ created by compositing
-- 'c1' over 'c2'.
over :: Colour -> Colour -> Colour
over = C.over

-- | Blends a @'Colour'@ with black, without changing its opacity.
darken :: R -> Colour -> Colour
darken = C.darken

-- | @'dissolve' k@ returns a @'Colour'@ more transparent by a
-- factor of @k@.
dissolve :: R -> Colour -> Colour
dissolve = C.darken

-- | Construct an opaque colour from red, green, blue in [0,1].
cRGB :: R -> R -> R -> Colour
cRGB r g b = cRGBA r g b 1.0

-- | Construct an opaque colour from hue in [0,360) and saturation,
-- value in [0,1].
cHSV :: R -> R -> R -> Colour
cHSV h s v = cHSVA h s v 1.0

-- | Construct an opaque colour from a gray level in [0,1].
cGray :: R -> Colour
cGray g = cGrayA g 1.0

-- | Construct a colour from red, green, blue, alpha in [0,1].
cRGBA :: R -> R -> R -> R -> Colour
cRGBA r g b a = withOpacity (C.sRGB r g b) a

-- | Construct a colour from hue in [0,360) and saturation,
-- value, alpha in [0,1].
cHSVA :: R -> R -> R -> R -> Colour
cHSVA h s v a = withOpacity (C.uncurryRGB C.sRGB (C.hsv h s v)) a

-- | Construct a colour from gray level and alpha in [0,1].
cGrayA :: R -> R -> Colour
cGrayA g a = cRGBA g g g a
