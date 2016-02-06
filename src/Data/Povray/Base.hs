module Data.Povray.Base where

type Str = String -- may change to ByteString or Text

join :: [Str] -> Str
join = unlines . filter (not . null)

class Povray a where
    toPov :: a -> Str
