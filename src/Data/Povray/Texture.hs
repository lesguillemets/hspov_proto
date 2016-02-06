module Data.Povray.Texture where

import Data.Povray.Base
import Data.Povray.Types

data Texture = Texture Str

instance Povray Texture where
    toPov (Texture x) = "texture!"
