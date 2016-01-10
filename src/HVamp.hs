-- HVamp - A Vamp host for Haskell
--
-- Copyright (C) 2014, 2015 Richard Lewis, Goldsmiths' College
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

module HVamp where

import Control.Applicative
import Control.Exception (bracket)
import Data.Traversable (traverse)
import Foreign
import Foreign.C.String
import Foreign.Storable (peek)
import Host
import Vamp

type Library = String
type Index = Integer
type PluginName = String
type PluginID = (Library, Index, PluginName)

listLibraries :: IO [Library]
listLibraries = do
  count <- c_getLibraryCount
  traverse (\n -> c_getLibraryName n >>= peekCString) [0..(count - 1)]

listPluginsOfLib :: Ptr VHLibrary -> Library -> IO [PluginID]
listPluginsOfLib libPtr libName = do
  let pluginId n = do
        pdPtr <- c_getPluginDescriptor libPtr (fromInteger n)
        pd <- peek pdPtr
        return $ (libName, n, pldName pd)
  count <- c_getPluginCount libPtr
  traverse pluginId [0..((toInteger count) - 1)]

listPlugins :: IO [[PluginID]]
listPlugins = do
  let withLib n f = bracket
                    (do { l <- c_loadLibrary n; return l })
                    (\l -> c_unloadLibrary l)
                    (\l -> (f n l))
      findPluginIDs n l = do
        c <- c_getPluginCount l
        libName <- (c_getLibraryName n) >>= peekCString
        listPluginsOfLib l libName

  count <- c_getLibraryCount
  traverse (\n -> withLib n findPluginIDs) [0..(count - 1)]

loadPlugin :: Library -> Index -> IO (Maybe HVPluginDescriptorPtr)
loadPlugin = undefined
