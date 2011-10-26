{-# LANGUAGE
    DeriveDataTypeable #-}
module Propane.Types
    ( R, R2, Time, Angle, Count
    , Colour, Image, Animation
    , Size(..), Raster(..), Rastimation(..), Word8
    ) where

import qualified Data.Colour     as C
import qualified Data.Array.Repa as R
import qualified Data.Sequence   as S

import Data.Word
import Data.Data ( Typeable, Data )

type R     = Float
type R2    = (R, R)
type Time  = R
type Angle = R
type Count = Int

type Colour = C.AlphaColour R

type Image     a = R2 -> a
type Animation a = Time -> Image a

data Size = Size
    { sWidth  :: Int
    , sHeight :: Int }
    deriving (Eq, Ord, Read, Show, Typeable, Data)

newtype Raster = Raster (R.Array R.DIM3 Word8)
    deriving (Eq, Show, Typeable)

newtype Rastimation = Rastimation (S.Seq Raster)
    deriving (Eq, Show, Typeable)
