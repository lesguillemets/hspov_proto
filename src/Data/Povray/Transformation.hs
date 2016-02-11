{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Data.Povray.Transformation where

import Data.Povray.Base
import Data.Povray.Types

data Transformation = Transformation {
    _rotate :: Maybe Vect,
    _scale :: Maybe Vect,
    _translate :: Maybe Vect
}

emptyTransformation :: Transformation
emptyTransformation = Transformation Nothing Nothing Nothing

-- FIXME : Order
instance Povray Transformation where
    toPov Transformation{..} = join[
        maybeToPovWithName "translate" _translate,
        maybeToPovWithName "rotate" _rotate,
        maybeToPovWithName "scale" _scale
        ]
