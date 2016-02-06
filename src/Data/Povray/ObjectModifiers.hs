{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
-- TODO : http://www.povray.org/documentation/3.7.0/r3_4.html
module Data.Povray.ObjectModifiers where

import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Texture
import Data.Povray.Transformation

import Data.Maybe

data ObjectModifier = OModify {
    _texture :: Maybe Texture,
    _trans :: Maybe Transformation,
    _comment :: Maybe Str
}

emptyModifier :: ObjectModifier
emptyModifier = OModify Nothing Nothing Nothing

instance Povray ObjectModifier where
    toPov (m@OModify{..}) = unlines [
        fromMaybe "" $ toPov <$> _texture,
        fromMaybe "" $ toPov <$> _trans,
        fromMaybe "" $ formComment <$> _comment
        ]
