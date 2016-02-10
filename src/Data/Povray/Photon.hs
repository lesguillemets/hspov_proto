{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Data.Povray.Photon where

import Data.Povray.Base
import Data.Povray.Types

import Data.Maybe

data Photon = Photon {
    photonSpacing :: Maybe Double,
    photonCount :: Maybe Int,
    -- photonGather :: Maybe (Int,Int)
    photonMedia :: Maybe PhotonMedia,
    -- photonJitter :: Maybe Double,
    photonMaxTraceLevel :: Maybe Int,
    -- photonADCBailout :: Maybe Double,
    -- photonSaveFile :: Maybe Str,
    -- photonLoadFile :: Maybe Str,
    photonAutoStop :: Maybe Double,
    photonExpandThresholds :: Maybe (Double, Double)
    -- photon radius
}
emptyPhoton :: Photon
emptyPhoton = Photon Nothing Nothing Nothing Nothing Nothing Nothing

instance Povray Photon where
    toPov Photon{..} =
        join [
            "photons {",
            maybeToPovWithName "spacing" photonSpacing,
            maybeToPovWithName "count" photonCount,
            maybeToPov photonMedia,
            maybeToPovWithName "max_trace_level" photonMaxTraceLevel,
            maybeToPovWithName "autostop" photonAutoStop,
            petToPov photonExpandThresholds,
            "}"
            ]

petToPov :: Maybe (Double,Double) -> Str
petToPov Nothing = ""
petToPov (Just (a,b))
    = mconcat ["expand_thresholds ", toPov a, " , ", toPov b]

data PhotonMedia = PhotonMedia {
    pmMaxSteps :: Int,
    pmFactor :: Maybe Double
}

instance Povray PhotonMedia where
    toPov (PhotonMedia sm f) =
        ("media " `mappend` toPov sm `mappend`)
            . fromMaybe "" $ (((", " `mappend`) . toPov) <$> f)
