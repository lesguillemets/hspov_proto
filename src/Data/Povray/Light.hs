{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Data.Povray.Light where

import Data.Povray.Types
import Data.Povray.Base
import Data.Povray.Colour

import Data.Maybe

data Light = Light {
    lLoc :: Vect,
    lColour :: Colour,
    lKind :: Maybe LightKind
}

data LightKind = Point
               | Spotlight {
                   pointAt :: Vect,
                   radius :: Maybe Double,
                   falloff :: Maybe Double,
                   tightness :: Maybe Double
               }
               | CylinderLight {
                   pointAt :: Vect,
                   radius :: Maybe Double,
                   falloff :: Maybe Double,
                   tightness :: Maybe Double
               }
               | ParallelLight {
                   pointAt :: Vect
               }
               | AreaLight {
                   axis0 :: Vect,
                   axis1 :: Vect,
                   size0 :: Int,
                   size1 :: Int,
                   adaptive :: Maybe Int,
                   jitter :: Bool,
                   circular :: Bool,
                   orient :: Bool
                   -- TODO : Cylinder / spotlight
               }

instance Povray LightKind where
    toPov Spotlight{..}
        = join [
            "spotlight",
            "point_at " `mappend` toPov pointAt,
            fromMaybe "" $ (("radius " `mappend`) . toPov) <$> radius,
            fromMaybe "" $ (("falloff " `mappend`) . toPov) <$> falloff,
            fromMaybe "" $ (("tightness " `mappend`) . toPov) <$> tightness
            ]
    toPov CylinderLight{..}
        = join [
            "cylinder",
            "point_at " `mappend` toPov pointAt,
            fromMaybe "" $ (("radius " `mappend`) . toPov) <$> radius,
            fromMaybe "" $ (("falloff " `mappend`) . toPov) <$> falloff,
            fromMaybe "" $ (("tightness " `mappend`) . toPov) <$> tightness
            ]
    toPov ParallelLight{..}
        = join [
            "parallel",
            "point_at " `mappend` toPov pointAt
            ]
    toPov AreaLight{..}
        = join [
            "area_light " `mappend` toPov axis0 `mappend` ", "
                `mappend` toPov axis1 `mappend` ", "
                `mappend` toPov size0 `mappend` ", " `mappend` toPov size1,
            fromMaybe "" $ (("adaptive " `mappend`) . toPov) <$> adaptive,
            if jitter then "jitter" else "",
            if circular then "circular" else "",
            if orient then "orient" else ""
            ]

instance Povray Light where
    toPov (Light l c k) =
        join [
            "light_source {",
            toPov l,
            toPov c,
            maybeToPov k,
            "}"
        ]
