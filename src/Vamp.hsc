-- HVamp - A Vamp host for Haskell
--
-- Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College
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

{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vamp ( HVPluginHandle
            , HVPluginDescriptor
            , HVPluginDescriptorPtr
            , pldName
            , pluginInstantiate
            , pluginCleanup
            , pluginInitialise
            , pluginReset
            , pluginGetParameter
            , pluginSetParameter
            , pluginGetCurrentProgram
            , pluginSelectProgram
            , pluginGetPreferredStepSize
            , pluginGetPreferredBlockSize
            , pluginGetMinChannelCount
            , pluginGetMaxChannelCount
            , pluginGetOutputCount
            , pluginGetOutputDescriptor
            , pluginReleaseOutputDescriptor
            , pluginProcess
            , pluginProcessVector
            , pluginGetRemainingFeatures
            , pluginReleaseFeatureSet
            )
       where

import           Control.Monad (forM_)
import           Data.Char (chr)
import           Data.List (intercalate)
import qualified Data.Vector.Storable as DV
import qualified Data.Vector.Storable.Mutable as DVM
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

#include "vamp/vamp.h"
#include "vamp-hostsdk/host-c.h"

data HVParameterDescriptor = HVParameterDescriptor {
  pdIdentifier   :: String,
  pdName         :: String,
  pdDescription  :: String,
  pdUnit         :: String,
  pdMinValue     :: Float,
  pdMaxValue     :: Float,
  pdDefaultValue :: Float,
  pdIsQuantized  :: Bool,
  pdQuantizeStep :: Maybe Float,
  pdValueNames   :: [String] } deriving (Eq, Show)
type HVParameterDescriptorPtr = Ptr (HVParameterDescriptor)

newtype HVSampleType = HVSampleType { unHVSampleType :: CInt } deriving (Eq, Show)

#{enum HVSampleType, HVSampleType
 , stOneSamplePerStep   = vampOneSamplePerStep
 , stFixedSampleRate    = vampFixedSampleRate
 , stVariableSampleRate = vampVariableSampleRate
 }

data HVOutputDescriptor = HVOutputDescriptor {
  odIdentifier       :: String,
  odName             :: String,
  odDescription      :: String,
  odUnit             :: String,
  odHasFixedBinCount :: Bool,
  odBinCount         :: Int,
  odBinNames         :: [String],
  odHasKnownExtents  :: Bool,
  odMinValue         :: Float,
  odMaxValue         :: Float,
  odIsQuantized      :: Bool,
  odQuantizeStep     :: Maybe Float,
  odSampleType       :: HVSampleType,
  odSampleRate       :: Float,
  odHasDuration      :: Bool } deriving (Eq, Show)
type HVOutputDescriptorPtr = Ptr (HVOutputDescriptor)

data HVFeature = HVFeature {
  fHasTimestamp :: Bool,
  fSec          :: Int,
  fNSec         :: Int,
  fValueCount   :: Int,
  fValues       :: DV.Vector Float,
  fLabel        :: Maybe String } deriving (Eq, Show)
type HVFeaturePtr = Ptr (HVFeature)

data HVFeatureV2 = HVFeatureV2 {
  f2HasDuration  :: Bool,
  f2DurationSec  :: Int,
  f2DurationNSec :: Int } deriving (Eq, Show)
type HVFeatureV2Ptr = Ptr (HVFeatureV2)

-- In vamp.h there's a union type along these lines; creating a
-- storable instance for this will be complicated as we'd need to know
-- at peek/poke time which kind of value is in the union
--data Feature = Feature | FeatureV2 deriving (Eq, Show)

data HVFeatureList = HVFeatureListV1 { flv1FeatureCount :: Int, flv1Features :: DV.Vector HVFeature }
                   | HVFeatureListV2 { flv2FeatureCount :: Int,
                                       flv2V1Features :: DV.Vector HVFeature,
                                       flv2V2Features :: DV.Vector HVFeatureV2 }
                   deriving (Eq, Show)
type HVFeatureListPtr = Ptr (HVFeatureList)

newtype HVInputDomain = HVInputDomain { unHVInputDomain :: CInt } deriving (Eq, Show)

#{enum HVInputDomain, HVInputDomain
 , idTimeDomain      = vampTimeDomain
 , idFrequencyDomain = vampFrequencyDomain
 }

-- This corresponds to typedef void *VampPluginHandle; in
-- vamp.h. Doing it this way (rather than creating an ADT with a
-- single constructor) keeps foreign import "dynamic" happy.
type HVPluginHandle = Ptr ()

type HVInstantiateFun = (HVPluginDescriptorPtr -> CFloat -> IO HVPluginHandle)
type HVCleanupFun = (HVPluginHandle -> IO ())
type HVInitialiseFun = (HVPluginHandle -> CInt -> CInt -> CInt -> IO CInt)
type HVResetFun = (HVPluginHandle -> IO ())
type HVGetParameterFun = (HVPluginHandle -> CInt -> IO CFloat)
type HVSetParameterFun = (HVPluginHandle -> CInt -> CFloat -> IO ())
type HVGetCurrentProgramFun = (HVPluginHandle -> IO CUInt)
type HVSelectProgramFun = (HVPluginHandle -> CUInt -> IO ())
type HVGetPreferredStepSizeFun = (HVPluginHandle -> IO CUInt)
type HVGetPreferredBlockSizeFun = (HVPluginHandle -> IO CUInt)
type HVGetMinChannelCountFun = (HVPluginHandle -> IO CUInt)
type HVGetMaxChannelCountFun = (HVPluginHandle -> IO CUInt)
type HVGetOutputCountFun = (HVPluginHandle -> IO CUInt)
type HVGetOutputDescriptorFun = (HVPluginHandle -> CUInt -> IO HVOutputDescriptorPtr)
type HVReleaseOutputDescriptorFun = (HVPluginHandle -> HVOutputDescriptorPtr -> IO ())
type HVProcessFun = (HVPluginHandle -> Ptr (Ptr CFloat) -> CInt -> CInt -> IO HVFeatureListPtr)
type HVGetRemainingFeaturesFun = (HVPluginHandle -> IO HVFeatureListPtr)
type HVReleaseFeatureSetFun = (HVPluginHandle -> IO ())

data HVPluginDescriptor = HVPluginDescriptor {
  pldVampApiVersion :: Int,
  pldIdentifier     :: String,
  pldName           :: String,
  pldDescription    :: String,
  pldMaker          :: String,
  pldPluginVersion  :: Int,
  pldCopyright      :: String,
  pldParameterCount :: Int,
  pldParameters     :: [HVParameterDescriptor],
  pldProgramCount   :: Int,
  pldPrograms       :: [String],
  pldInputDomain    :: HVInputDomain,
  pldInstantiate             :: HVInstantiateFun, -- HVPluginDescriptorPtr -> CFloat -> IO HVPluginHandle,
  pldCleanup                 :: HVCleanupFun, -- HVPluginHandle -> IO (),
  pldInitialise              :: HVInitialiseFun, -- HVPluginHandle -> CInt -> CInt -> CInt -> IO CInt,
  pldReset                   :: HVResetFun, -- HVPluginHandle -> IO (),
  pldGetParameter            :: HVGetParameterFun, -- HVPluginHandle -> CInt -> IO CFloat,
  pldSetParameter            :: HVSetParameterFun, -- HVPluginHandle -> CInt -> CFloat -> IO (),
  pldGetCurrentProgram       :: HVGetCurrentProgramFun, -- HVPluginHandle -> IO CUInt,
  pldSelectProgram           :: HVSelectProgramFun, -- HVPluginHandle -> CUInt -> IO (),
  pldGetPreferredStepSize    :: HVGetPreferredStepSizeFun, -- HVPluginHandle -> IO CUInt,
  pldGetPreferredBlockSize   :: HVGetPreferredBlockSizeFun, -- HVPluginHandle -> IO CUInt,
  pldGetMinChannelCount      :: HVGetMinChannelCountFun, -- HVPluginHandle -> IO CUInt,
  pldGetMaxChannelCount      :: HVGetMaxChannelCountFun, -- HVPluginHandle -> IO CUInt,
  pldGetOutputCount          :: HVGetOutputCountFun, -- HVPluginHandle -> IO CUInt,
  pldGetOutputDescriptor     :: HVGetOutputDescriptorFun, -- HVPluginHandle -> CUInt -> IO HVOutputDescriptorPtr,
  pldReleaseOutputDescriptor :: HVReleaseOutputDescriptorFun, -- HVPluginHandle -> HVOutputDescriptorPtr -> IO (),
  pldProcess                 :: HVProcessFun, -- HVPluginHandle -> Ptr CFloat -> CInt -> CInt -> IO HVFeatureListPtr,
  pldGetRemainingFeatures    :: HVGetRemainingFeaturesFun, -- HVPluginHandle -> IO HVFeatureListPtr,
  pldReleaseFeatureSet       :: HVReleaseFeatureSetFun } -- HVPluginHandle -> IO () }
type HVPluginDescriptorPtr = Ptr (HVPluginDescriptor)

instance Show HVPluginDescriptor where
  show HVPluginDescriptor { pldIdentifier = identifier, pldName = name } = "HVPluginDescriptor [" ++ (show identifier) ++ "] " ++ (show name)

instance Eq HVPluginDescriptor where
  (==) HVPluginDescriptor { pldIdentifier = a } HVPluginDescriptor { pldIdentifier = b } = a == b

foreign import ccall "dynamic"
  mkHVInstantiateFun :: FunPtr HVInstantiateFun -> HVInstantiateFun
foreign import ccall "dynamic"
  mkHVCleanupFun :: FunPtr HVCleanupFun -> HVCleanupFun
foreign import ccall "dynamic"
  mkHVInitialiseFun :: FunPtr HVInitialiseFun -> HVInitialiseFun
foreign import ccall "dynamic"
  mkHVResetFun :: FunPtr HVResetFun -> HVResetFun
foreign import ccall "dynamic"
  mkHVGetParameterFun :: FunPtr HVGetParameterFun -> HVGetParameterFun
foreign import ccall "dynamic"
  mkHVSetParameterFun :: FunPtr HVSetParameterFun -> HVSetParameterFun
foreign import ccall "dynamic"
  mkHVGetCurrentProgramFun :: FunPtr HVGetCurrentProgramFun -> HVGetCurrentProgramFun
foreign import ccall "dynamic"
  mkHVSelectProgramFun :: FunPtr HVSelectProgramFun -> HVSelectProgramFun
foreign import ccall "dynamic"
  mkHVGetPreferredStepSizeFun :: FunPtr HVGetPreferredStepSizeFun -> HVGetPreferredStepSizeFun
foreign import ccall "dynamic"
  mkHVGetPreferredBlockSizeFun :: FunPtr HVGetPreferredBlockSizeFun -> HVGetPreferredBlockSizeFun
foreign import ccall "dynamic"
  mkHVGetMinChannelCountFun :: FunPtr HVGetMinChannelCountFun -> HVGetMinChannelCountFun
foreign import ccall "dynamic"
  mkHVGetMaxChannelCountFun :: FunPtr HVGetMaxChannelCountFun -> HVGetMaxChannelCountFun
foreign import ccall "dynamic"
  mkHVGetOutputCountFun :: FunPtr HVGetOutputCountFun -> HVGetOutputCountFun
foreign import ccall "dynamic"
  mkHVGetOutputDescriptorFun :: FunPtr HVGetOutputDescriptorFun -> HVGetOutputDescriptorFun
foreign import ccall "dynamic"
  mkHVReleaseOutputDescriptorFun :: FunPtr HVReleaseOutputDescriptorFun -> HVReleaseOutputDescriptorFun
foreign import ccall "dynamic"
  mkHVProcessFun :: FunPtr HVProcessFun -> HVProcessFun
foreign import ccall "dynamic"
  mkHVGetRemainingFeaturesFun :: FunPtr HVGetRemainingFeaturesFun -> HVGetRemainingFeaturesFun
foreign import ccall "dynamic"
  mkHVReleaseFeatureSetFun :: FunPtr HVReleaseFeatureSetFun -> HVReleaseFeatureSetFun

type HVGetPluginDescriptor = FunPtr (CUInt -> CUInt -> IO HVPluginDescriptorPtr)

splitCStrL :: Int -> Ptr CChar -> IO [String]
splitCStrL n p
  | p == nullPtr = return []
  | otherwise = splt p []
  where
    splt :: Ptr CChar -> [String] -> IO [String]
    splt ptr acc = do
      s <- peekCString ptr
      l <- if (length acc) < n then splt (ptr `plusPtr` (length s)) (s : acc) else return acc
      return (reverse l)

splitCStrL0 :: Ptr CChar -> IO [String]
splitCStrL0 p
  | p == nullPtr = return []
  | otherwise = splt p []
  where
    splt :: Ptr CChar -> [String] -> IO [String]
    splt ptr acc = do
      s <- peekCString ptr
      l <- if ptr == nullPtr || s == "" then return acc else splt (ptr `plusPtr` (length s)) (s : acc)
      return (reverse l)

joinCStrL :: [String] -> IO CString
joinCStrL ss = do
  let s = intercalate [(chr 0)] ss
  cs <- newCString s
  return cs

isTrueCInt :: CInt -> IO Bool
isTrueCInt i = do
  return (i == (1 :: CInt))

peekCStringOrEmpty :: CString -> IO String
peekCStringOrEmpty c = if c == nullPtr then return "" else peekCString c

instance Storable HVParameterDescriptor where
  alignment _ = alignment (undefined :: CFloat)
  sizeOf _    = #{size VampParameterDescriptor}

  peek pd = do

    identifier'  <- (#peek VampParameterDescriptor, identifier) pd  >>= peekCStringOrEmpty
    name'        <- (#peek VampParameterDescriptor, name) pd        >>= peekCStringOrEmpty
    description' <- (#peek VampParameterDescriptor, description) pd >>= peekCStringOrEmpty
    unit'        <- (#peek VampParameterDescriptor, unit) pd        >>= peekCStringOrEmpty

    minValue'     <- fmap realToFrac (((#peek VampParameterDescriptor, minValue) pd) :: IO CFloat)
    maxValue'     <- fmap realToFrac (((#peek VampParameterDescriptor, maxValue) pd) :: IO CFloat)
    defaultValue' <- fmap realToFrac (((#peek VampParameterDescriptor, defaultValue) pd) :: IO CFloat)

    isQuantized'  <- (#peek VampParameterDescriptor, isQuantized) pd >>= isTrueCInt
    quantizeStep' <- fmap realToFrac (((#peek VampParameterDescriptor, quantizeStep) pd) :: IO CFloat)

    valueNames'   <- (#peek VampParameterDescriptor, valueNames) pd >>= splitCStrL0

    return HVParameterDescriptor { pdIdentifier   = identifier',
                                   pdName         = name',
                                   pdDescription  = description',
                                   pdUnit         = unit',
                                   pdMinValue     = minValue',
                                   pdMaxValue     = maxValue',
                                   pdDefaultValue = defaultValue',
                                   pdIsQuantized  = isQuantized',
                                   pdQuantizeStep = if isQuantized' then Just quantizeStep' else Nothing,
                                   pdValueNames   = valueNames' }

  poke pd (HVParameterDescriptor { pdIdentifier = identifier', pdName = name', pdDescription = description',
                                   pdUnit = unit', pdMinValue = minValue', pdMaxValue = maxValue',
                                   pdDefaultValue = defaultValue', pdIsQuantized = isQuantized',
                                   pdQuantizeStep = quantizeStep', pdValueNames = valueNames' }) = do
    newCString identifier'  >>= (#poke VampParameterDescriptor, identifier) pd
    newCString name'        >>= (#poke VampParameterDescriptor, name) pd
    newCString description' >>= (#poke VampParameterDescriptor, description) pd
    newCString unit'        >>= (#poke VampParameterDescriptor, unit) pd

    (#poke VampParameterDescriptor, minValue) pd minValue'
    (#poke VampParameterDescriptor, maxValue) pd maxValue'
    (#poke VampParameterDescriptor, defaultValue) pd defaultValue'

    (#poke VampParameterDescriptor, isQuantized) pd (if isQuantized' then 1 :: CInt else 0 :: CInt)
    case quantizeStep' of
     Just q  -> (#poke VampParameterDescriptor, quantizeStep) pd q
     Nothing -> (#poke VampParameterDescriptor, quantizeStep) pd (0 :: Float)

    joinCStrL valueNames' >>= (#poke VampParameterDescriptor, valueNames) pd

instance Storable HVOutputDescriptor where
  alignment _ = alignment (undefined :: CFloat)
  sizeOf _    = #{size VampOutputDescriptor}

  peek od = do
    identifier'  <- (#peek VampOutputDescriptor, identifier) od  >>= peekCString
    name'        <- (#peek VampOutputDescriptor, name) od        >>= peekCString
    description' <- (#peek VampOutputDescriptor, description) od >>= peekCString
    unit'        <- (#peek VampOutputDescriptor, unit) od        >>= peekCString

    hasFixedBinCount'  <- (#peek VampOutputDescriptor, hasFixedBinCount) od >>= isTrueCInt
    binCount'          <- fmap fromIntegral (((#peek VampOutputDescriptor, binCount) od) :: IO CInt)
    binNames'          <- (#peek VampOutputDescriptor, binNames) od >>= splitCStrL0

    hasKnownExtents' <- (#peek VampOutputDescriptor, hasKnownExtents) od >>= isTrueCInt
    minValue'        <- fmap realToFrac (((#peek VampOutputDescriptor, minValue) od) :: IO CFloat)
    maxValue'        <- fmap realToFrac (((#peek VampOutputDescriptor, maxValue) od) :: IO CFloat)

    isQuantized'  <- (#peek VampOutputDescriptor, isQuantized) od >>= isTrueCInt
    quantizeStep' <- fmap realToFrac (((#peek VampOutputDescriptor, quantizeStep) od) :: IO CFloat)

    sampleTypeInt   <- ((#peek VampOutputDescriptor, sampleType) od) :: IO CInt
    let sampleType' = HVSampleType { unHVSampleType = sampleTypeInt }

    sampleRate' <- fmap realToFrac (((#peek VampOutputDescriptor, sampleRate) od) :: IO CFloat)

    hasDuration' <- (#peek VampOutputDescriptor, hasDuration) od >>= isTrueCInt

    return HVOutputDescriptor { odIdentifier = identifier',
                                odName = name',
                                odDescription = description',
                                odUnit = unit',
                                odHasFixedBinCount = hasFixedBinCount',
                                odBinCount = binCount',
                                odBinNames = binNames',
                                odHasKnownExtents = hasKnownExtents',
                                odMinValue = minValue',
                                odMaxValue = maxValue',
                                odIsQuantized = isQuantized',
                                odQuantizeStep = if isQuantized' then Just quantizeStep' else Nothing,
                                odSampleType = sampleType',
                                odSampleRate = sampleRate',
                                odHasDuration = hasDuration' }

  poke od (HVOutputDescriptor { odIdentifier = identifier', odName = name', odDescription = description',
                                odUnit = unit', odHasFixedBinCount = hasFixedBinCount', odBinCount = binCount',
                                odBinNames = binNames', odHasKnownExtents = hasKnownExtents',
                                odMinValue = minValue', odMaxValue = maxValue', odIsQuantized = isQuantized',
                                odQuantizeStep = quantizeStep', odSampleType = sampleType',
                                odSampleRate = sampleRate', odHasDuration = hasDuration' }) = do
    newCString identifier'  >>= (#poke VampOutputDescriptor, identifier) od
    newCString name'        >>= (#poke VampOutputDescriptor, name) od
    newCString description' >>= (#poke VampOutputDescriptor, description) od
    newCString unit'        >>= (#poke VampOutputDescriptor, unit) od

    (#poke VampOutputDescriptor, hasFixedBinCount) od (if hasFixedBinCount' then 1 :: CInt else 0 :: CInt)
    (#poke VampOutputDescriptor, binCount) od binCount'
    joinCStrL binNames' >>= (#poke VampOutputDescriptor, binNames) od

    (#poke VampOutputDescriptor, hasKnownExtents) od (if hasKnownExtents' then 1 :: CInt else 0 :: CInt)
    (#poke VampOutputDescriptor, minValue) od minValue'
    (#poke VampOutputDescriptor, maxValue) od maxValue'

    (#poke VampOutputDescriptor, isQuantized) od (if isQuantized' then 1 :: CInt else 0 :: CInt)
    case quantizeStep' of
     Just q  -> (#poke VampOutputDescriptor, quantizeStep) od q
     Nothing -> (#poke VampOutputDescriptor, quantizeStep) od (0 :: Float)

    (#poke VampOutputDescriptor, sampleType) od (unHVSampleType sampleType')
    (#poke VampOutputDescriptor, sampleRate) od sampleRate'

    (#poke VampOutputDescriptor, hasDuration) od (if hasDuration' then 1 :: CInt else 0 :: CInt)

instance Storable HVFeature where
  alignment _ = alignment (undefined :: CFloat)
  sizeOf _    = #{size VampFeature}

  peek f = do
    hasTimestamp'  <- (#peek VampFeature, hasTimestamp) f >>= isTrueCInt
    sec'           <- fmap fromIntegral (((#peek VampFeature, sec) f) :: IO CInt)
    nSec'          <- fmap fromIntegral (((#peek VampFeature, nsec) f) :: IO CInt)

    valueCount' <- fmap fromIntegral (((#peek VampFeature, valueCount) f) :: IO CUInt)
    valuesField <- ((#peek VampFeature, values) f) :: IO (Ptr Float)
    valuesPtr   <- newForeignPtr_ valuesField
    let values' = DV.unsafeFromForeignPtr0 valuesPtr valueCount'

    labelPtr <- ((#peek VampFeature, label) f) :: IO CString
    label''  <- peekCString labelPtr
    let label' = if labelPtr == nullPtr then Nothing else Just label''

    return HVFeature { fHasTimestamp = hasTimestamp',
                       fSec          = sec',
                       fNSec         = nSec',
                       fValueCount   = valueCount',
                       fValues       = values',
                       fLabel        = label' }

  poke f (HVFeature { fHasTimestamp = hasTimestamp', fSec = sec', fNSec = nSec',
                      fValueCount = valueCount', fValues = values', fLabel = label' }) = do
    (#poke VampFeature, hasTimestamp) f (if hasTimestamp' then 1 :: CInt else 0 :: CInt)
    (#poke VampFeature, sec) f sec'
    (#poke VampFeature, nsec) f nSec'

    (#poke VampFeature, valueCount) f valueCount'
    DV.unsafeWith values' (\ptrValues -> (#poke VampFeature, values) f ptrValues)

    case label' of
     Just l  -> newCString l >>= (#poke VampFeature, label) f
     Nothing -> (#poke VampFeature, label) f nullPtr

instance Storable HVFeatureV2 where
  alignment _ = alignment (undefined :: CInt)
  sizeOf _    = #{size VampFeatureV2}

  peek f2 = do
    hasDuration'  <- (#peek VampFeatureV2, hasDuration) f2 >>= isTrueCInt
    durationSec'  <- fmap fromIntegral (((#peek VampFeatureV2, durationSec) f2) :: IO CInt)
    durationNSec' <- fmap fromIntegral (((#peek VampFeatureV2, durationNsec) f2) :: IO CInt)

    return HVFeatureV2 { f2HasDuration  = hasDuration',
                         f2DurationSec  = durationSec',
                         f2DurationNSec = durationNSec' }

  poke f2 (HVFeatureV2 { f2HasDuration = hasDuration', f2DurationSec = durationSec', f2DurationNSec = durationNSec' }) = do
    (#poke VampFeatureV2, hasDuration) f2 (if hasDuration' then 1 :: CInt else 0 :: CInt)
    (#poke VampFeatureV2, durationSec) f2 durationSec'
    (#poke VampFeatureV2, durationNsec) f2 durationNSec'

instance Storable HVFeatureList where
  alignment _ = alignment (undefined :: CFloat)
  sizeOf _    = #{size VampFeatureList}

  peek fl = do
    featureCount' <- fmap fromIntegral (((#peek VampFeatureList, featureCount) fl) :: IO CUInt)

    featuresPtr  <- ((#peek VampFeatureList, features) fl) :: IO (Ptr HVFeature)
    featuresFPtr <- newForeignPtr_ featuresPtr
    let v2FeaturesPtr = plusPtr featuresPtr (featureCount' * #{size VampFeature})

    return $ if v2FeaturesPtr == nullPtr
             then HVFeatureListV1 { flv1FeatureCount = featureCount',
                                    flv1Features = DV.unsafeFromForeignPtr0 featuresFPtr featureCount' }
             else HVFeatureListV2 { flv2FeatureCount = featureCount',
                                    flv2V1Features = DV.unsafeFromForeignPtr0 featuresFPtr featureCount',
                                    flv2V2Features = DV.unsafeFromForeignPtr (castForeignPtr featuresFPtr) featureCount' featureCount' }

  poke _ _ = error "Cannot poke a HVFeatureList"

peekArrayPtrs :: Storable a => Int -> Ptr (Ptr a) -> IO [a]
peekArrayPtrs size ptr | size <= 0 = return []
                       | otherwise = f (size-1) []
  where
    f 0 acc = do ePtr <- peekElemOff ptr 0; e <- peek ePtr; return (e:acc)
    f n acc = do ePtr <- peekElemOff ptr n; e <- peek ePtr; f (n-1) (e:acc)

instance Storable HVPluginDescriptor where
  alignment _ = alignment (undefined :: CFloat)
  sizeOf _    = #{size VampPluginDescriptor}

  peek pld = do
    apiVersion' <- fmap fromIntegral (((#peek VampPluginDescriptor, vampApiVersion) pld) :: IO CUInt)

    identifier'    <- (#peek VampPluginDescriptor, identifier) pld  >>= peekCString
    name'          <- (#peek VampPluginDescriptor, name) pld        >>= peekCString
    description'   <- (#peek VampPluginDescriptor, description) pld >>= peekCString
    maker'         <- (#peek VampPluginDescriptor, maker) pld       >>= peekCString
    pluginVersion' <- fmap fromIntegral (((#peek VampPluginDescriptor, pluginVersion) pld) :: IO CUInt)
    copyright'     <- (#peek VampPluginDescriptor, copyright) pld   >>= peekCString

    parameterCount' <- fmap fromIntegral (((#peek VampPluginDescriptor, parameterCount) pld) :: IO CUInt)
    parameters'     <- if parameterCount' > 0
                       then (((#peek VampPluginDescriptor, parameters) pld) :: IO (Ptr HVParameterDescriptorPtr)) >>= peekArrayPtrs parameterCount'
                       else return []

    programCount' <- fmap fromIntegral (((#peek VampPluginDescriptor, programCount) pld) :: IO CUInt)
    programs'     <- ((#peek VampPluginDescriptor, programs) pld) >>= splitCStrL programCount'

    inputDomainInt   <- ((#peek VampPluginDescriptor, inputDomain) pld) :: IO CInt
    let inputDomain' = HVInputDomain { unHVInputDomain = inputDomainInt }

    instantiatePtr             <- ((#peek VampPluginDescriptor, instantiate) pld) :: IO (FunPtr HVInstantiateFun)
    cleanupPtr                 <- ((#peek VampPluginDescriptor, cleanup) pld) :: IO (FunPtr HVCleanupFun)
    initialisePtr              <- ((#peek VampPluginDescriptor, initialise) pld) :: IO (FunPtr HVInitialiseFun)
    resetPtr                   <- ((#peek VampPluginDescriptor, reset) pld) :: IO (FunPtr HVResetFun)
    getParameterPtr            <- ((#peek VampPluginDescriptor, getParameter) pld) :: IO (FunPtr HVGetParameterFun)
    setParameterPtr            <- ((#peek VampPluginDescriptor, setParameter) pld) :: IO (FunPtr HVSetParameterFun)
    getCurrentProgramPtr       <- ((#peek VampPluginDescriptor, getCurrentProgram) pld) :: IO (FunPtr HVGetCurrentProgramFun)
    selectProgramPtr           <- ((#peek VampPluginDescriptor, selectProgram) pld) :: IO (FunPtr HVSelectProgramFun)
    getPreferredStepSizePtr    <- ((#peek VampPluginDescriptor, getPreferredStepSize) pld) :: IO (FunPtr HVGetPreferredStepSizeFun)
    getPreferredBlockSizePtr   <- ((#peek VampPluginDescriptor, getPreferredBlockSize) pld) :: IO (FunPtr HVGetPreferredBlockSizeFun)
    getMinChannelCountPtr      <- ((#peek VampPluginDescriptor, getMinChannelCount) pld) :: IO (FunPtr HVGetMinChannelCountFun)
    getMaxChannelCountPtr      <- ((#peek VampPluginDescriptor, getMaxChannelCount) pld) :: IO (FunPtr HVGetMaxChannelCountFun)
    getOutputCountPtr          <- ((#peek VampPluginDescriptor, getOutputCount) pld) :: IO (FunPtr HVGetOutputCountFun)
    getOutputDescriptorPtr     <- ((#peek VampPluginDescriptor, getOutputDescriptor) pld) :: IO (FunPtr HVGetOutputDescriptorFun)
    releaseOutputDescriptorPtr <- ((#peek VampPluginDescriptor, releaseOutputDescriptor) pld) :: IO (FunPtr HVReleaseOutputDescriptorFun)
    processPtr                 <- ((#peek VampPluginDescriptor, process) pld) :: IO (FunPtr HVProcessFun)
    getRemainingFeaturesPtr    <- ((#peek VampPluginDescriptor, getRemainingFeatures) pld) :: IO (FunPtr HVGetRemainingFeaturesFun)
    releaseFeatureSetPtr       <- ((#peek VampPluginDescriptor, releaseFeatureSet) pld) :: IO (FunPtr HVReleaseFeatureSetFun)

    return HVPluginDescriptor { pldVampApiVersion = apiVersion',
                                pldIdentifier     = identifier',
                                pldName           = name',
                                pldDescription    = description',
                                pldMaker          = maker',
                                pldPluginVersion  = pluginVersion',
                                pldCopyright      = copyright',
                                pldParameterCount = parameterCount',
                                pldParameters     = parameters',
                                pldProgramCount   = programCount',
                                pldPrograms       = programs',
                                pldInputDomain    = inputDomain',
                                pldInstantiate             = mkHVInstantiateFun instantiatePtr,
                                pldCleanup                 = mkHVCleanupFun cleanupPtr,
                                pldInitialise              = mkHVInitialiseFun initialisePtr,
                                pldReset                   = mkHVResetFun resetPtr,
                                pldGetParameter            = mkHVGetParameterFun getParameterPtr,
                                pldSetParameter            = mkHVSetParameterFun setParameterPtr,
                                pldGetCurrentProgram       = mkHVGetCurrentProgramFun getCurrentProgramPtr,
                                pldSelectProgram           = mkHVSelectProgramFun selectProgramPtr,
                                pldGetPreferredStepSize    = mkHVGetPreferredStepSizeFun getPreferredStepSizePtr,
                                pldGetPreferredBlockSize   = mkHVGetPreferredBlockSizeFun getPreferredBlockSizePtr,
                                pldGetMinChannelCount      = mkHVGetMinChannelCountFun getMinChannelCountPtr,
                                pldGetMaxChannelCount      = mkHVGetMaxChannelCountFun getMaxChannelCountPtr,
                                pldGetOutputCount          = mkHVGetOutputCountFun getOutputCountPtr,
                                pldGetOutputDescriptor     = mkHVGetOutputDescriptorFun getOutputDescriptorPtr,
                                pldReleaseOutputDescriptor = mkHVReleaseOutputDescriptorFun releaseOutputDescriptorPtr,
                                pldProcess                 = mkHVProcessFun processPtr,
                                pldGetRemainingFeatures    = mkHVGetRemainingFeaturesFun getRemainingFeaturesPtr,
                                pldReleaseFeatureSet       = mkHVReleaseFeatureSetFun releaseFeatureSetPtr }

  poke _ _ = error "Cannot poke a HVPluginDescriptor"

pluginInstantiate :: HVPluginDescriptorPtr -> HVPluginDescriptor -> CFloat -> IO HVPluginHandle
pluginInstantiate descPtr (HVPluginDescriptor { pldInstantiate = instantiate }) sampleRate = instantiate descPtr sampleRate

pluginCleanup :: HVPluginDescriptor -> HVPluginHandle -> IO ()
pluginCleanup (HVPluginDescriptor { pldCleanup = cleanup }) = cleanup

pluginInitialise :: HVPluginDescriptor -> HVPluginHandle -> CInt -> CInt -> CInt -> IO CInt
pluginInitialise (HVPluginDescriptor { pldInitialise = initialise }) = initialise

pluginReset :: HVPluginDescriptor -> HVPluginHandle -> IO ()
pluginReset (HVPluginDescriptor { pldReset = reset }) = reset

pluginGetParameter :: HVPluginDescriptor -> HVPluginHandle -> CInt -> IO CFloat
pluginGetParameter (HVPluginDescriptor { pldGetParameter = getParameter }) = getParameter

pluginSetParameter :: HVPluginDescriptor -> HVPluginHandle -> CInt -> CFloat -> IO ()
pluginSetParameter (HVPluginDescriptor { pldSetParameter = setParameter }) = setParameter

pluginGetCurrentProgram :: HVPluginDescriptor -> HVPluginHandle -> IO CUInt
pluginGetCurrentProgram (HVPluginDescriptor { pldGetCurrentProgram = getCurrentProgram }) = getCurrentProgram

pluginSelectProgram :: HVPluginDescriptor -> HVPluginHandle -> CUInt -> IO ()
pluginSelectProgram (HVPluginDescriptor { pldSelectProgram = selectProgram }) = selectProgram

pluginGetPreferredStepSize :: HVPluginDescriptor -> HVPluginHandle -> IO CUInt
pluginGetPreferredStepSize (HVPluginDescriptor { pldGetPreferredStepSize = getPreferredStepSize }) = getPreferredStepSize

pluginGetPreferredBlockSize :: HVPluginDescriptor -> HVPluginHandle -> IO CUInt
pluginGetPreferredBlockSize (HVPluginDescriptor { pldGetPreferredBlockSize = getPreferredBlockSize }) = getPreferredBlockSize

pluginGetMinChannelCount :: HVPluginDescriptor -> HVPluginHandle -> IO CUInt
pluginGetMinChannelCount (HVPluginDescriptor { pldGetMinChannelCount = getMinChannelCount }) = getMinChannelCount

pluginGetMaxChannelCount :: HVPluginDescriptor -> HVPluginHandle -> IO CUInt
pluginGetMaxChannelCount (HVPluginDescriptor { pldGetMaxChannelCount = getMaxChannelCount }) = getMaxChannelCount

pluginGetOutputCount :: HVPluginDescriptor -> HVPluginHandle -> IO CUInt
pluginGetOutputCount (HVPluginDescriptor { pldGetOutputCount = getOutputCount }) = getOutputCount

pluginGetOutputDescriptor :: HVPluginDescriptor -> HVPluginHandle -> CUInt -> IO HVOutputDescriptorPtr
pluginGetOutputDescriptor (HVPluginDescriptor { pldGetOutputDescriptor = getOutputDescriptor }) = getOutputDescriptor

pluginReleaseOutputDescriptor :: HVPluginDescriptor -> HVPluginHandle -> HVOutputDescriptorPtr -> IO ()
pluginReleaseOutputDescriptor (HVPluginDescriptor { pldReleaseOutputDescriptor = releaseOutputDescriptor }) = releaseOutputDescriptor

pluginProcess :: HVPluginDescriptor -> HVPluginHandle -> Ptr (Ptr CFloat) -> CInt -> CInt -> IO HVFeatureListPtr
pluginProcess (HVPluginDescriptor { pldProcess = process }) = process

pluginProcessVector :: HVPluginDescriptor -> HVPluginHandle -> [DV.Vector CFloat] -> CInt -> CInt -> IO HVFeatureListPtr
pluginProcessVector (HVPluginDescriptor { pldProcess = process }) plgH buffers sec nsec = do
  input <- DVM.new (length buffers)
  forM_ (zip buffers [0..]) $ \(b, i) -> DV.unsafeWith b $ (\ptr -> DVM.unsafeWrite input i ptr)
  DVM.unsafeWith input $ \ptr -> do process plgH ptr sec nsec

pluginGetRemainingFeatures :: HVPluginDescriptor -> HVPluginHandle -> IO HVFeatureListPtr
pluginGetRemainingFeatures (HVPluginDescriptor { pldGetRemainingFeatures = getRemainingFeatures }) = getRemainingFeatures

pluginReleaseFeatureSet :: HVPluginDescriptor -> HVPluginHandle -> IO ()
pluginReleaseFeatureSet (HVPluginDescriptor { pldReleaseFeatureSet = releaseFeatureSet }) = releaseFeatureSet
