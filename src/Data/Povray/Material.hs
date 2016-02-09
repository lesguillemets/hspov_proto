{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Data.Povray.Material where

import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Texture

data Interior = NamedInterior Str
instance Povray Interior where
    toPov (NamedInterior s) = "interior {" `mappend` s `mappend` " }"

-- TODO : Interior Texture
data Material = Material { mTexture :: Maybe Texture,
                           mInterior :: Maybe Interior
                         }
              | NamedMaterial { materialName :: Str }

instance Povray Material where
    toPov (NamedMaterial s) = "material { " `mappend` s `mappend` " }"
    toPov (m@Material{..})
        = join [
            "material {",
            maybeToPov mTexture,
            maybeToPov mInterior,
            "}"
            ]
