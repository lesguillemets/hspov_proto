module Data.Povray.Camera where

import Data.Povray.Types
import Data.Povray.Base

data Camera = Camera {
    _location :: Vect,
    _lookAt :: Vect
}

instance Povray Camera where
    toPov (Camera loc at) = unlines [
        "camera {",
        "location " ++ toPov loc,
        "look_at" ++ toPov loc
        ]
