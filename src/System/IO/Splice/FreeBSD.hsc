{- | Exposes the FreeBSD system call @setsockopt@, specifically for use with option @SO_SPLICE@.

     /This module is only available (compiled & exposed) on FreeBSD 14.2-RELEASE or greater.
-}
--
-- Module      : System.IO.Splice.FreeBSD
-- Copyright   : (c) Christopher Milan 2025
-- License     : BSD3
-- Maintainer  : chrismilan@ucla.edu
-- Stability   : stable
-- Portability : FreeBSD-only


#include <sys/socket.h>
#include <sys/types.h>
#include <sys/time.h>
{-# LANGUAGE CPP, ForeignFunctionInterface #-}


module System.IO.Splice.FreeBSD (

    sOL_SOCKET,
    sO_SPLICE,
    StructTimeval(..),
    StructSplice(..)

  ) where


import Data.Int
import Foreign.C.Types
import System.Posix.Types
import Foreign.Storable
import Control.Monad (ap)


sOL_SOCKET :: Word
sOL_SOCKET = (#const "SOL_SOCKET")

sO_SPLICE :: Word
sO_SPLICE = (#const "SO_SPLICE")

data StructTimeval = StructTimeval {
  tv_sec :: (#type time_t),
  tv_usec :: (#type suseconds_t)
}

instance Storable StructTimeval where
  sizeOf    _ = #{size struct timeval}
  alignment _ = alignment (undefined :: CInt)

  poke p tv = do
    #{poke struct timeval, tv_sec} p $ tv_sec tv
    #{poke struct timeval, tv_usec} p $ tv_usec tv

  peek p = return StructTimeval
           `ap` (#{peek struct timeval, tv_sec} p)
           `ap` (#{peek struct timeval, tv_usec} p)

data StructSplice = StructSplice {
  -- | fd to splice to
  sp_fd :: Fd,
  -- | maximum bytes to send
  sp_max :: (#type off_t),
  -- | timeout to unsplice
  sp_idle :: StructTimeval
}

instance Storable StructSplice where
  sizeOf    _ = #{size struct splice}
  alignment _ = alignment (undefined :: CInt)

  poke p splice = do
    #{poke struct splice, sp_fd} p $ sp_fd splice
    #{poke struct splice, sp_max} p $ sp_max splice
    #{poke struct splice, sp_idle} p $ sp_idle splice

  peek p = return StructSplice
           `ap` (#{peek struct splice, sp_fd} p)
           `ap` (#{peek struct splice, sp_max} p)
           `ap` (#{peek struct splice, sp_idle} p)

