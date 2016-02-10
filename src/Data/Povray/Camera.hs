module Data.Povray.Camera where

import Data.Povray.Types
import Data.Povray.Base

import Data.Maybe

data Camera = Camera {
    _location :: Vect,
    _lookAt :: Vect,
    _angle :: Maybe Double,
    _sky :: Maybe Vect
}

instance Povray Camera where
    toPov (Camera loc at ang sky) = join [
        "camera {",
        "location " `mappend` toPov loc,
        maybeToPovWithName "angle" ang,
        maybeToPovWithName "sky" sky,
        "look_at " `mappend` toPov at,
        "}"
        ]
