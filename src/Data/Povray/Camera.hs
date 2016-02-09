module Data.Povray.Camera where

import Data.Povray.Types
import Data.Povray.Base

data Camera = Camera {
    _location :: Vect,
    _lookAt :: Vect
}

instance Povray Camera where
    toPov (Camera loc at) = join [
        "camera {",
        "location " `mappend` toPov loc,
        "look_at " `mappend` toPov at,
        "}"
        ]
