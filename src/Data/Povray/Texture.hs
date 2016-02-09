module Data.Povray.Texture where

import Data.Povray.Base
import Data.Povray.Colour
import Data.Povray.Pigment
import Data.Povray.Types

import Data.Maybe


data Finish = Phong Double
            | Ambient Double -- TODO

instance Povray Finish where
    toPov (Phong x) = "finish { phong " `mappend` toPov x `mappend` " }"
    toPov (Ambient x) = "finish { ambient " `mappend` toPov x `mappend` " }"

data Texture = Texture { _pigment :: Maybe Pigment,
                         _finish :: Maybe Finish }
             | NamedTexture {textureName :: Str}

emptyTexture :: Texture
emptyTexture = Texture Nothing Nothing

instance Povray Texture where
    toPov (Texture p f) =
        join [
            "texture {",
            maybeToPov p,
            maybeToPov f,
            "}"
            ]
    toPov (NamedTexture n) = "texture {" `mappend` n `mappend` "}"
