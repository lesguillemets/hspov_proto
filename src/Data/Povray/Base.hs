module Data.Povray.Base where

type Str = String -- may change to ByteString or Text

class Povray a where
    toPov :: a -> Str
