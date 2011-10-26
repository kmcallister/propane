module Propane.Helpers
    ( saveImage
    , saveAnimation
    ) where

import Propane.Types
import Propane.Raster
import Propane.IO

saveImage :: FilePath -> Size -> Image Colour -> IO ()
saveImage name sz = saveRaster name . rasterize sz

saveAnimation :: FilePath -> Count -> Size -> Animation Colour -> IO ()
saveAnimation name nFrames sz = saveRastimation name . rastimate nFrames sz
