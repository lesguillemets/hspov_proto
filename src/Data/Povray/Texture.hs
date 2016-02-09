{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Data.Povray.Texture where

import Data.Povray.Base
import Data.Povray.Colour
import Data.Povray.Types

import Data.Maybe

data Pigment = Pigment {
    _color :: Maybe Colour,
    _named :: Maybe NamedPigment
}

emptyPigment :: Pigment
emptyPigment = Pigment Nothing Nothing

instance Povray Pigment where
    toPov (p@Pigment{..}) =
        case join [
                fromMaybe "" $ toPov <$> _color,
                fromMaybe "" $ toPov <$> _named
                ] of
                    "" -> ""
                    s -> join ["pigment {", s, "}"]

-- TODO : This is too ad hoc
data NamedPigment = Checker (Maybe Colour) (Maybe Colour)
                  | Named Str
instance Povray NamedPigment where
    toPov (Checker c0 c1) =
        join [ "checker",
               fromMaybe "" $ toPov <$> c0,
               fromMaybe "" $ toPov <$> c1
            ]
    toPov (Named s) = s


data Finish = Phong Double
            | Ambient Double -- TODO

instance Povray Finish where
    toPov (Phong x) = "finish { phong " `mappend` toPov x `mappend` " }"
    toPov (Ambient x) = "finish { ambient " `mappend` toPov x `mappend` " }"

data Texture = Texture {
    _pigment :: Maybe Pigment,
    _finish :: Maybe Finish
}

emptyTexture :: Texture
emptyTexture = Texture Nothing Nothing

instance Povray Texture where
    toPov (Texture p f) =
        join [
            "texture {",
            fromMaybe "" $ toPov <$> p,
            fromMaybe "" $ toPov <$> f,
            "}"
            ]
