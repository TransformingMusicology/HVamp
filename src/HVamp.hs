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

{-# LANGUAGE ForeignFunctionInterface #-}
module HVamp (  listPlugins
              , pluginKey
              , pluginPath
              , pluginCategory
              , loadPlugin )
       where

import Vamp
import Foreign
import Foreign.C.Types
import Foreign.C.String

type PluginCategory = String
type PluginKey = String

data AdaptorFlag = AdaptorFlag
data Plugin = Plugin

-- PluginLoader.h: PluginLoader::listPlugings
foreign import ccall unsafe "vamp.h hvamp_list_plugins"
  c_list_plugins :: IO (Ptr CString)

listPlugins :: IO [PluginKey]
listPlugins = undefined

-- PluginLoader.h: PluginLoader::composePluginKey
foreign import ccall unsafe "vamp.h hvamp_plugin_key"
  c_plugin_key :: CString -> CString -> IO CString

pluginKey :: FilePath -> String -> IO PluginKey
pluginKey = undefined

-- PluginLoader.h: PluginLoader::getLibraryPathForPlugin
foreign import ccall unsafe "vamp.h hvamp_plugin_path"
  c_plugin_path :: CString -> IO CString

pluginPath :: PluginKey -> IO FilePath
pluginPath = undefined

-- PluginLoader.h: PluginLoader::getPluginCategory
foreign import ccall unsafe "vamp.h hvamp_plugin_category"
  c_plugin_category :: CString -> IO (Ptr CString)

pluginCategory :: PluginKey -> IO [PluginCategory]
pluginCategory = undefined

-- PluginLoader.h: PluginLoader::loadPlugin
foreign import ccall unsafe "vamp.h hvamp_load_plugin"
  c_load_plugin :: CString -> CFloat -> CInt -> IO HVPluginHandle

loadPlugin :: PluginKey -> Float -> AdaptorFlag -> IO HVPluginHandle
loadPlugin = undefined
