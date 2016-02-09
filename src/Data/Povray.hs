module Data.Povray where

import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Objects
import Data.Povray.Texture
import Data.Povray.Transformation

include :: Str -> IO ()
include = putStrLn . ("#include " `mappend`) . show

put :: Povray a => a -> IO ()
put = putStrLn . toPov
