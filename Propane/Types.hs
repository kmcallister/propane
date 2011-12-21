{-# LANGUAGE
    DeriveDataTypeable #-}
-- | Types for functional image synthesis.
module Propane.Types
    ( -- * Basic type synonyms
      --
      -- | These exist to improve type signatures
      -- as documentation.
      R, R2, Time, Angle, Count

      -- * Images and animations
    , Colour, Image, Animation

      -- * Rasterization
      --
      -- | Images and animations can be converted to
      -- discrete objects, e.g. for output to files.
    , Size(..), Raster(..), Rastimation(..), Word8
    ) where

import qualified Data.Colour     as C
import qualified Data.Array.Repa as R
import qualified Data.Sequence   as S

import Data.Word
import Data.Data ( Typeable, Data )

-- | The (fake) real numbers.
--
-- This is the only place where we choose between @'Float'@ and @'Double'@.
-- Switching to @'Double'@ would increase demands on memory bandwidth and
-- cache space, decreasing performance (by 30% in one vaguely relevant
-- test).
type R = Float

-- | Cartesian coordinates (/x/,/y/), representing points in the plane.
type R2 = (R, R)

-- | Time, in no particular unit.
type Time = R

-- | An angle, in radians.
type Angle = R

-- | A count, e.g. number of repetitions of something.
type Count = Int

-- | A colour with alpha channel.
--
-- @'C.AlphaColour'@ is defined in the @colour@ package, which also provides
-- many functions for working with this type.
type Colour = C.AlphaColour R

-- | An image provides a value (such as colour) for every point of the
-- real plane.
type Image a = R2 -> a

-- | An animation is a time-varying image.
type Animation a = Time -> Image a

-- | The number of pixels in a raster image.
data Size = Size
    { sWidth  :: Count
    , sHeight :: Count }
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A @'Raster'@ is a finite rectangle of an @'Image'@, with space and colour
-- reduced to discrete quantities.
--
-- It is represented by a Repa @'R.Array'@ with indices of the form
-- (@Z :. y :. x :. c@) where:
--
-- * (@x@, @y@) are pixel counts in the Cartesian /x/ and /y/ directions respectively.
--
-- * @c@ is a colour channel index: 0, 1, 2, 3 for red, green, blue, alpha respectively.
newtype Raster = Raster (R.Array R.DIM3 Word8)
    deriving (Eq, Show, Typeable)

-- | A @'Rastimation'@ is an animation of @'Raster'@ frames, where time has
-- also been reduced to a discrete quantity.
--
-- It is represented as a sequence of @'Raster'@s.
newtype Rastimation = Rastimation (S.Seq Raster)
    deriving (Eq, Show, Typeable)
