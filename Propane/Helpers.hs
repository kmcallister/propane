-- | Helpfully combines functionality from other modules.
module Propane.Helpers
    ( saveImage
    , saveAnimation
    ) where

import Propane.Types
import Propane.Raster
import Propane.IO

-- | Save an image.  Invokes @'rasterize'@ and @'saveRaster'@.
saveImage :: FilePath -> Size -> Image Colour -> IO ()
saveImage name sz = saveRaster name . rasterize sz

-- | Save an animation.  Invakes @'rastimate'@ and @'saveRastimation'@.
saveAnimation :: FilePath -> Count -> Size -> Animation Colour -> IO ()
saveAnimation name nFrames sz = saveRastimation name . rastimate nFrames sz
