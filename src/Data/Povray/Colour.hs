module Data.Povray.Colour where

import Data.Povray.Base
import Data.Povray.Types

data Colour = RGB Vect
            | RGBF Vect4
            | RGBT Vect4
            | RGBFT Vect5

-- TODO : better polymorphism?

cmul :: Double -> Colour -> Colour
cmul x (RGB v)   = RGB (fmap (*x) v)
cmul x (RGBF v)  = RGBF (fmap (*x) v)
cmul x (RGBT v)  = RGBT (fmap (*x) v)
cmul x (RGBFT v) = RGBFT (fmap (*x) v)

instance Povray Colour where
    toPov (RGB v) = "colour rgb " `mappend` toPov v
    toPov (RGBF v) = "colour rgbf " `mappend` toPov v
    toPov (RGBT v) = "colour rgbt " `mappend` toPov v
    toPov (RGBFT v) = "colour rgbft " `mappend` toPov v
-- |
-- >>> toPov (RGB (V 1 0.4 0.3))
-- colour rgb <1, 0.4, 0.3>
