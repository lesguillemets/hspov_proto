module Data.Povray.Transformation where

import Data.Povray.Base
import Data.Povray.Types

data Transformation = Transformation Str

instance Povray Transformation where
    toPov (Transformation x) = "trans!"
