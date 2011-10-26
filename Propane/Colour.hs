module Propane.Colour
    ( BaseColour
    , opaque, transparent, withOpacity
    , blend, over, darken, dissolve
    ) where

import qualified Data.Colour as C

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
