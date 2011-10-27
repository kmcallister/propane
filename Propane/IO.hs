{-# LANGUAGE
    TemplateHaskell #-}
-- | Input and output.
--
-- TODO: input.
--
-- XXX FIXME XXX: This module is not safe for multithreaded use
-- with GHC 7.2 or earlier, due to this bug:
--
-- <http://hackage.haskell.org/trac/ghc/ticket/5558>
--
-- This will be fixed before Propane is released to Hackage.

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

-- | Save the @'Raster'@ to a given file.
--
-- The file format is specified by the filename, and can
-- be any of the formats supported by the DevIL library.
saveRaster :: FilePath -> Raster -> IO ()
saveRaster name (Raster img) = do
    evaluate (R.deepSeqArray img ())
    withMVar devilLock $ \() ->
        D.runIL (D.writeImage name img)

-- | Save the @'Rastimation'@ to a sequence of frames in
-- the given directory.
--
-- The frames will be PNG files with names like
--
-- >00000000.png
-- >00000001.png
--
-- etc, in frame order.
--
-- Files are written concurrently, and there is no guarantee
-- about which files exist, until the IO action completes.
saveRastimation :: FilePath -> Rastimation -> IO ()
saveRastimation dir (Rastimation frames) = do
    createDirectoryIfMissing True dir
    -- Check existence, to give better error messages
    e <- doesDirectoryExist dir
    when (not e)
        (throwIO . ErrorCall $ errStr ("directory does not exist: " ++ dir))

    let eval :: Raster -> IO ()
        eval (Raster img) = evaluate (R.deepSeqArray img ())
        go :: Int -> Raster -> IO ()
        go i img = saveRaster (dir </> printf "%08d.png" i) img

    let frs = F.toList frames
    parMapIO_ eval     frs
    zipWithM_ go [0..] frs
