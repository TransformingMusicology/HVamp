-- HVAMP - A Vamp host for Haskell
--
-- Copyright (C) 2015 Richard Lewis, Goldsmiths' College
-- Author: Richard Lewis <richard.lewis@gold.ac.uk>

{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Vamp where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include "hvamp.h"

data HVParameterDescriptor = HVParameterDescriptor {
  pdIdentifier   :: String,
  pdName         :: String,
  pdDescription  :: String,
  pdUnit         :: String,
  pdMinValue     :: Float,
  pdMaxValue     :: Float,
  pdDefaultValue :: Float,
  pdIsQuantized  :: Bool,
  pdQuantizeStep :: Float,
  pdValueNames   :: Maybe [String] } deriving (Eq, Show)
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
  odHasFixedBinCount :: Int,
  odBinCount         :: Int,
  odBinNames         :: [String],
  odHasKnownExtents  :: Int,
  odMinValue         :: Float,
  odMaxValue         :: Float,
  odIsQuantized      :: Bool,
  odQuantizeStep     :: Float,
  odSampleType       :: HVSampleType,
  odSampleRate       :: Float,
  odHasDuration      :: Int } deriving (Eq, Show)
type HVOutputDescriptorPtr = Ptr (HVOutputDescriptor)

data HVFeature = HVFeature {
  fHasTimestamp :: Bool,
  fSec          :: Int,
  fNSec         :: Int,
  fValueCount   :: Int,
  fValues       :: [Float],
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
data Feature = Feature | FeatureV2 deriving (Eq, Show)

data HVFeatureList = HVFeatureList {
  flFeatureCount :: Int,
  flFeatures     :: [Feature] -- in vamp.h it's an array of VampFeatureUnion
  } deriving (Eq, Show)
type HVFeatureListPtr = Ptr (HVFeatureList)

newtype HVInputDomain = HVInputDomain { unHVInputDomain :: CInt } deriving (Eq, Show)

#{enum HVInputDomain, HVInputDomain
 , idTimeDomain      = vampTimeDomain
 , idFrequencyDomain = vampFrequencyDomain
 }

data HVPluginHandle = HVPluginHandle
type HVPluginHandlePtr = Ptr (HVPluginHandle)

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
  pldInstantiate             :: FunPtr (HVPluginDescriptorPtr -> CFloat -> IO HVPluginHandle),
  pldCleanup                 :: FunPtr (HVPluginHandle -> IO ()),
  pldInitialise              :: FunPtr (HVPluginHandle -> CInt -> CInt -> CInt -> IO CInt),
  pldReset                   :: FunPtr (HVPluginHandle -> IO ()),
  pldGetParameter            :: FunPtr (HVPluginHandle -> CInt -> IO CFloat),
  pldSetParameter            :: FunPtr (HVPluginHandle -> CInt -> CFloat -> IO ()),
  pldGetCurrentProgram       :: FunPtr (HVPluginHandle -> IO CUInt),
  pldSelectProgram           :: FunPtr (HVPluginHandle -> CUInt -> IO ()),
  pldGetPreferredStepSize    :: FunPtr (HVPluginHandle -> IO CUInt),
  pldGetPreferredBlockSize   :: FunPtr (HVPluginHandle -> IO CUInt),
  pldGetMinChannelCount      :: FunPtr (HVPluginHandle -> IO CUInt),
  pldGetMaxChannelCount      :: FunPtr (HVPluginHandle -> IO CUInt),
  pldGetOutputCount          :: FunPtr (HVPluginHandle -> IO CUInt),
  pldGetOutputDescriptor     :: FunPtr (HVPluginHandle -> CUInt -> IO HVOutputDescriptorPtr),
  pldReleaseOutputDescriptor :: FunPtr (HVPluginHandle -> HVOutputDescriptorPtr -> IO ()),
  pldProcess                 :: FunPtr (HVPluginHandle -> [[Float]] -> Int -> Int -> IO HVFeatureListPtr),
  pldGetRemainingFeatures    :: FunPtr (HVPluginHandle -> IO HVFeatureListPtr),
  pldReleaseFeatureSet       :: FunPtr (HVPluginHandle -> IO ()) }
type HVPluginDescriptorPtr = Ptr (HVPluginDescriptor)

instance Show HVPluginDescriptor where
  show HVPluginDescriptor { pldIdentifier = identifier, pldName = name } = "HVPluginDescriptor [" ++ (show identifier) ++ "] " ++ (show name)

instance Eq HVPluginDescriptor where
  (==) HVPluginDescriptor { pldIdentifier = a } HVPluginDescriptor { pldIdentifier = b } = a == b

type HVGetPluginDescriptor = FunPtr (CUInt -> CUInt -> IO HVPluginDescriptorPtr)

foreign import ccall "vamp.h vampGetPluginDescriptor"
  c_vampGetPluginDescriptor :: CUInt -> CUInt -> IO HVPluginDescriptorPtr
