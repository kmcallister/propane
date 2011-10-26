module Propane.Helpers
    ( saveImage
    , spaced
    ) where

import Propane.Types
import Propane.Raster
import Propane.IO

saveImage :: FilePath -> Size -> Image Colour -> IO ()
saveImage name sz = saveRaster name . rasterize sz

spaced :: (Fractional a) => Count -> a -> a -> [a]
spaced n a b = take n (iterate (+i) a) where
    i = (b - a) / fromIntegral n
