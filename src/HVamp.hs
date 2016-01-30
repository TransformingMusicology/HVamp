-- HVamp - A Vamp host for Haskell
--
-- Copyright (C) 2014-2016 Richard Lewis, Goldsmiths' College
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

module HVamp ( listLibraries
             , listPlugins
             , listPluginsOfLib
             , loadMaybePlugin
             , instantiateMaybePlugin
             , initialiseMaybePluginP
             , initialiseMaybePlugin
             , withMaybePluginHandle
             , withMaybePluginHandle_ ) where

import Control.Applicative
import Control.Exception (bracket)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Foreign
import Foreign.C.String
import Foreign.C.Types (CFloat(..))
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
  let
    pluginId n = do
      pdPtr <- c_getPluginDescriptor libPtr (fromInteger n)
      pd <- peek pdPtr
      return $ (libName, n, pldName pd)
  count <- c_getPluginCount libPtr
  traverse pluginId [0..((toInteger count) - 1)]

listPlugins :: IO [[PluginID]]
listPlugins = do
  let
    withLib n f =
      bracket
        (do { l <- c_loadLibrary n; return l })
        (\l -> c_unloadLibrary l)
        (\l -> (f n l))
    findPluginIDs n l = do
      libName <- (c_getLibraryName n) >>= peekCString
      listPluginsOfLib l libName

  count <- c_getLibraryCount
  traverse (\n -> withLib n findPluginIDs) [0..(count - 1)]

loadMaybePluginDescPtr :: PluginID -> IO (Maybe HVPluginDescriptorPtr)
loadMaybePluginDescPtr (libName, pluginIdx, _) = do
  libNameC <- newCString libName
  libIdx   <- c_getLibraryIndex libNameC
  if libIdx < 0 then return Nothing else do
    libPtr <- c_loadLibrary libIdx
    if libPtr == nullPtr then return Nothing else do
      plgPtr <- c_getPluginDescriptor libPtr (fromIntegral pluginIdx)
      return $ if plgPtr /= nullPtr then Just plgPtr else Nothing

loadMaybePlugin :: PluginID -> IO (Maybe HVPluginDescriptor)
loadMaybePlugin plgId = loadMaybePluginDescPtr plgId >>= peekDescriptor
  where
    peekDescriptor (Just ptr) = do { d <- peek ptr; return (Just d) }
    peekDescriptor Nothing    = return Nothing

instantiateMaybePlugin :: PluginID -> Float -> IO (Maybe HVPluginHandle)
instantiateMaybePlugin plgId sampleRate = loadMaybePluginDescPtr plgId >>= peekDescriptor
  where
    peekDescriptor (Just ptr) = instantiateMaybePluginFromDesc ptr sampleRate
    peekDescriptor Nothing    = return Nothing

instantiateMaybePluginFromDesc :: HVPluginDescriptorPtr -> Float -> IO (Maybe HVPluginHandle)
instantiateMaybePluginFromDesc plgDescPtr sampleRate = do
  desc <- peek plgDescPtr
  hndl <- pluginInstantiate plgDescPtr desc (CFloat sampleRate)
  if hndl /= nullPtr then return (Just hndl) else return Nothing

initialiseMaybePluginP :: HVPluginDescriptor -> Maybe HVPluginHandle -> Int -> Int -> Int -> IO Bool
initialiseMaybePluginP _ Nothing _ _ _ = return False
initialiseMaybePluginP desc (Just hndl) inputChannels stepSize blockSize = do
  minCh <- fmap fromIntegral (pluginGetMinChannelCount desc hndl)
  maxCh <- fmap fromIntegral (pluginGetMaxChannelCount desc hndl)
  if inputChannels < minCh || inputChannels > maxCh
    then fail $ "inputChannels out of range ([" ++ (show minCh) ++ ".." ++ (show maxCh) ++ "])"
    else do
      res <- pluginInitialise desc hndl (fromIntegral inputChannels) (fromIntegral stepSize) (fromIntegral blockSize)
      return $ if res == 0 then False else True

initialiseMaybePlugin :: HVPluginDescriptor -> Int -> Int -> Int -> Maybe HVPluginHandle -> IO (Maybe HVPluginHandle)
initialiseMaybePlugin desc inputChannels stepSize blockSize hndl =
  initialiseMaybePluginP desc hndl inputChannels stepSize blockSize >>= withP
  where
    withP True  = return hndl
    withP False = return Nothing

maybeM :: (Monad m) => b -> (a -> m b) -> Maybe a -> m b
maybeM def f = maybe (return def) f

maybeM_ :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
maybeM_ f x = maybeM () f x

withMaybePluginHandle :: PluginID -> Float -> Int -> Int -> Int -> (Maybe HVPluginDescriptor -> Maybe HVPluginHandle -> IO (Maybe a)) -> IO (Maybe a)
withMaybePluginHandle plgId sampleRate inputChannels stepSize blockSize f =
  loadMaybePluginDescPtr plgId >>= peekDescriptor
  where
    peekDescriptor (Just ptr) = do
      desc <- peek ptr
      bracket
        (instantiateMaybePluginFromDesc ptr sampleRate >>= initialiseMaybePlugin desc inputChannels stepSize blockSize)
        (maybeM_ (pluginCleanup desc))
        (f $ Just desc)
    peekDescriptor Nothing = return Nothing

withMaybePluginHandle_ :: PluginID -> Float -> Int -> Int -> Int -> (Maybe HVPluginDescriptor -> Maybe HVPluginHandle -> IO ()) -> IO ()
withMaybePluginHandle_ plgId sampleRate inputChannels stepSize blockSize f =
  withMaybePluginHandle plgId sampleRate inputChannels stepSize blockSize discardRes >> return ()
  where
    discardRes d h = do
      res <- f d h
      return $ Just undefined
