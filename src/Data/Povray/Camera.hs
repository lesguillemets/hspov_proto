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
        fromMaybe "" $ (("angle " `mappend`) . toPov) <$> ang,
        fromMaybe "" $ (("sky " `mappend`) . toPov) <$> sky,
        "look_at " `mappend` toPov at,
        "}"
        ]
