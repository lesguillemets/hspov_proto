module Data.Povray.Transformation where

import Data.Povray.Base
import Data.Povray.Types

data Transformation = Rotate Vect
                    | Scale Vect
                    | Translate Vect

instance Povray Transformation where
    toPov (Rotate v) = "rotate " `mappend` toPov v
    toPov (Scale v) = "scale " `mappend` toPov v
    toPov (Translate v) = "translate " `mappend` toPov v
