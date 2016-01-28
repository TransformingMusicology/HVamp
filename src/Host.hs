-- HVamp - A Vamp host for Haskell
--
-- Copyright (C) 2015 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

-- This file is part of HVamp

-- HVamp is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- HVamp is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with HVamp. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ForeignFunctionInterface #-}
module Host ( VHLibrary
            , c_getLibraryCount
            , c_getLibraryName
            , c_getLibraryIndex
            , c_loadLibrary
            , c_getPluginCount
            , c_getPluginDescriptor
            , c_unloadLibrary ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Vamp (HVPluginDescriptorPtr)

data VHLibrary = VHLibrary

foreign import ccall unsafe "host-c.h vhGetLibraryCount"
  c_getLibraryCount :: IO CInt

foreign import ccall unsafe "host-c.h vhGetLibraryName"
  c_getLibraryName :: CInt -> IO CString

foreign import ccall unsafe "host-c.h vhGetLibraryIndex"
  c_getLibraryIndex :: CString -> IO CInt

foreign import ccall unsafe "host-c.h vhLoadLibrary"
  c_loadLibrary :: CInt -> IO (Ptr VHLibrary)

foreign import ccall unsafe "host-c.h vhGetPluginCount"
  c_getPluginCount :: Ptr VHLibrary -> IO CInt

foreign import ccall unsafe "host-c.h vhGetPluginDescriptor"
  c_getPluginDescriptor :: Ptr VHLibrary -> CInt -> IO HVPluginDescriptorPtr

foreign import ccall unsafe "host-c.h vhUnloadLibrary"
  c_unloadLibrary :: Ptr VHLibrary -> IO ()
