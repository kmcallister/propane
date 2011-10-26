module Propane.Helpers
    ( saveImage
    ) where

import Propane.Types
import Propane.Raster
import Propane.IO

saveImage :: FilePath -> Size -> Image Colour -> IO ()
saveImage name sz = saveRaster name . rasterize sz
