{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
-- TODO : http://www.povray.org/documentation/3.7.0/r3_4.html#r3_4_5
module Data.Povray.Object where

import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Texture
import Data.Povray.Transformation
import Data.Povray.ObjectModifiers

import Data.Maybe

data Object = Box { _nll :: Vect, -- near lower left
                    _fur :: Vect, -- far upper right
                    _mod :: ObjectModifier }
            | Sphere { _centre :: Vect,
                       _rad :: Double,
                       _mod :: ObjectModifier }
            | Cone { _centre0 :: Vect, _rad0 :: Double,
                     _centre1 :: Vect, _rad1 :: Double,
                     _open :: Bool,
                     _mod :: ObjectModifier }
            | Cylinder { _centre0 :: Vect, _centre1 :: Vect,
                         _rad :: Double, _open :: Bool,
                         _mod :: ObjectModifier }
            | Plane { _norm :: Vect, _distFromO :: Double,
                      _mod :: ObjectModifier}
            | Torus { _major :: Double, _minor :: Double, _sturm :: Bool,
                      _mod :: ObjectModifier }
            | Union        {_objects :: [Object], _mod :: ObjectModifier}
            | Intersection {_objects :: [Object], _mod :: ObjectModifier}
            | Difference   {_objects :: [Object], _mod :: ObjectModifier}
            | Merge        {_objects :: [Object], _mod :: ObjectModifier}

isSingle :: Object -> Bool
isSingle Box{..} = True
isSingle Sphere{..} = True
isSingle _ = False

instance Povray Object where
    toPov b@Box{..} = join [
        "box {",
        toPov _nll,
        ",",
        toPov _fur,
        toPov _mod,
        "}"
        ]
    toPov s@Sphere{..} = join [
        "sphere {",
        toPov _centre,
        ",",
        toPov _rad,
        toPov _mod,
        "}"
        ]
    toPov c@Cone{..} = join [
        "cone {",
        toPov _centre0, ",", toPov _rad0,
        toPov _centre1, ",", toPov _rad1,
        if _open then "open" else "",
        toPov _mod,
        "}"
        ]
    toPov c@Cylinder{..} = join [
        "cylinder {",
        toPov _centre0, ",", toPov _centre1, ",", toPov _rad,
        if _open then "open" else "",
        toPov _mod,
        "}"
        ]
    toPov p@Plane{..} = join [
        "plane {",
        toPov _norm, ",", toPov _distFromO,
        toPov _mod,
        "}"
        ]
    toPov t@Torus{..} = join [
        "torus {",
        toPov _major, toPov _minor, if _sturm then "sturm" else "",
        toPov _mod,
        "}"
        ]
    toPov (Union os m) = unlines (
        "union { ": map toPov os
                                      ) `mappend` toPov m `mappend` "\n}"
    toPov (Intersection os m) = unlines (
        "intersection { ": map toPov os
                                      ) `mappend` toPov m `mappend` "\n}"
    toPov (Difference os m) = unlines (
        "difference { ": map toPov os
                                      ) `mappend` toPov m `mappend` "\n}"
    toPov (Merge os m) = unlines (
        "merge { ": map toPov os
                                      ) `mappend` toPov m `mappend` "\n}"
