{-# LANGUAGE FlexibleInstances #-}
module Data.Povray.Base where
import Data.Maybe
import Control.Monad (liftM)

type Str = String -- may change to ByteString or Text

join :: [Str] -> Str
join = unlines . filter (not . null)

class Povray a where
    toPov :: a -> Str

maybeToPov :: Povray a => Maybe a -> Str
maybeToPov = fromMaybe "" . liftM toPov

formComment :: Str -> Str
formComment c = let cs = lines c in
                if length cs == 1 then "// " `mappend` c
                                  else "/*\n" `mappend` c `mappend` "\n*/"

instance Povray Double where toPov = show
instance Povray [Char] where toPov = show
