{-# LANGUAGE
    ForeignFunctionInterface #-}
-- | Protect calls into DevIL with a global lock.
module Propane.IO.Lock
    ( lock
    ) where

import Foreign
import Foreign.C
import Control.Monad
import Control.Concurrent.MVar


foreign import ccall "hs_propane_get_global"
    c_get_global :: IO (Ptr ())

foreign import ccall "hs_propane_set_global"
    c_set_global :: Ptr () -> IO CInt


set :: IO ()
set = do
    mv  <- newMVar ()
    ptr <- newStablePtr mv
    ret <- c_set_global (castStablePtrToPtr ptr)
    when (ret == 0) $
        freeStablePtr ptr

get :: IO (MVar ())
get = do
    p <- c_get_global
    if p == nullPtr
        then set >> get
        else deRefStablePtr (castPtrToStablePtr p)

lock :: IO a -> IO a
lock act = get >>= flip withMVar (const act)
