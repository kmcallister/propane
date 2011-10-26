{-# LANGUAGE
    TemplateHaskell #-}
module Propane.IO
    ( saveRaster
    , saveRastimation
    ) where

import qualified Data.Array.Repa          as R
import qualified Data.Array.Repa.IO.DevIL as D

import qualified Data.Foldable as F

import Data.Global
import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Spawn
import Control.Exception
import System.FilePath
import System.Directory
import Text.Printf

import Propane.Types

-- Serialize access to DevIL, which isn't thread-safe
declareMVar  "devilLock"  [t| () |]  [e| () |]

errStr :: String -> String
errStr = ("Propane.IO: " ++)

saveRaster :: FilePath -> Raster -> IO ()
saveRaster name (Raster img) = do
    evaluate (R.deepSeqArray img ())
    withMVar devilLock $ \() ->
        D.runIL (D.writeImage name img)

saveRastimation :: FilePath -> Rastimation -> IO ()
saveRastimation dir (Rastimation frames) = do
    createDirectoryIfMissing True dir
    -- Check existence, to give better error messages
    e <- doesDirectoryExist dir
    when (not e)
        (throwIO . ErrorCall $ errStr ("directory does not exist: " ++ dir))

    let go :: Int -> Raster -> IO (IO ())
        go i img = spawn $ saveRaster (dir </> printf "%08d.png" i) img

    zipWithM go [0..] (F.toList frames) >>= sequence_
