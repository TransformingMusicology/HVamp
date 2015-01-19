-- HVamp - A Vamp host for Haskell
--
-- Copyright (C) 2014 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module HVamp where

data PluginCategory = PluginCategory

type PluginKey = String

-- PluginLoader.h: PluginLoader::listPlugings
listPlugins :: [PluginKey]
listPlugins = undefined

-- PluginLoader.h: PluginLoader::composePluginKey
pluginKey :: FilePath -> String -> PluginKey
pluginKey = undefined

-- PluginLoader.h: PluginLoader::getLibraryPathForPlugin
pluginPath :: PluginKey -> FilePath
pluginPath = undefined

-- PluginLoader.h: PluginLoader::getPluginCategory
pluginCategory :: PluginKey -> PluginCategory
pluginCategory = undefined

-- PluginLoader.h: PluginLoader::loadPlugin
loadPlugin :: PluginKey -> Float -> AdaptorFlag -> Maybe Plugin
loadPlugin = undefined
