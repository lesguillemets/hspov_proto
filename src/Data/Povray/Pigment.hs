{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Data.Povray.Pigment where

import Data.Povray.Base
import Data.Povray.Colour
import Data.Povray.Types

data Pigment = Pigment {
    _color :: Maybe Colour,
    _named :: Maybe NamedPigment
}

emptyPigment :: Pigment
emptyPigment = Pigment Nothing Nothing

instance Povray Pigment where
    toPov (p@Pigment{..}) =
        case join [
                maybeToPov _color,
                maybeToPov _named
                ] of
                    "" -> ""
                    s -> join ["pigment {", s, "}"]

-- TODO : This is too ad hoc
data NamedPigment = Checker (Maybe Colour) (Maybe Colour)
                  | Named Str
instance Povray NamedPigment where
    toPov (Checker c0 c1) =
        join [ "checker",
               maybeToPov c0,
               maybeToPov c1
            ]
    toPov (Named s) = s

