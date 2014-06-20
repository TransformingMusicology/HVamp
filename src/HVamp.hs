-- HVAMP - A Vamp host for Haskell
--
-- Copyright (C) 2014 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

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
