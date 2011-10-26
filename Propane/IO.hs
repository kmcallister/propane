{-# LANGUAGE
    TemplateHaskell #-}
module Propane.IO
    ( saveRaster
    ) where

import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D

import Data.Global
import Control.Concurrent.MVar
import Control.Exception ( evaluate )

import Propane.Types

-- Serialize access to DevIL, which isn't thread-safe
declareMVar  "devilLock"  [t| () |]  [e| () |]

saveRaster :: FilePath -> Raster -> IO ()
saveRaster name (Raster img) = do
    evaluate (R.deepSeqArray img ())
    withMVar devilLock $ \() ->
        D.runIL (D.writeImage name img)
