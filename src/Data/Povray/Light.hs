module Data.Povray.Light where

import Data.Povray.Types
import Data.Povray.Base
import Data.Povray.Colour

data Light = Light {
    lLoc :: Vect,
    lColour :: Colour
}
instance Povray Light where
    toPov (Light l c) =
        join [
            "light_source {",
            toPov l,
            toPov c,
            "}"
        ]
