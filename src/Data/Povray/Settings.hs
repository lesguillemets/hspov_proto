{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Data.Povray.Settings where

import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Colour
import Data.Povray.Photon

import Data.Maybe

data GlobalSetting = GlobalSetting {
    -- abcBailOut     :: Maybe Double,
    ambientLight      :: Maybe Colour,
    assumedGamma      :: Maybe Double,
    -- hfGray16       :: Bool,
    iridWaveLength    :: Maybe Colour,
    -- charSet        :: Maybe Charset,
    maxIntersections  :: Maybe Int,
    maxTraceLevel     :: Maybe Int,
    numberOfWaves     :: Maybe Int,
    -- noiseGenerator :: Maybe noiseGenerator,
    -- radiosity      :: Maybe Radiosity,
    photon            :: Maybe Photon
}

emptyGlobalSetting :: GlobalSetting
emptyGlobalSetting = GlobalSetting Nothing (Just 1.0) -- assumedGamma
                                   Nothing Nothing
                                   Nothing Nothing Nothing

instance Povray GlobalSetting where
    toPov GlobalSetting{..}
        = join[
            "global_settings {",
            maybeToPovWithName "ambient_light" ambientLight,
            maybeToPovWithName "assumed_gamma" assumedGamma,
            maybeToPovWithName "irid_wavelength" iridWaveLength,
            maybeToPovWithName "max_intersections" maxIntersections,
            maybeToPovWithName "max_trace_level" maxTraceLevel,
            maybeToPovWithName "number_of_waves" numberOfWaves,
            maybeToPov photon,
            "}"
            ]
