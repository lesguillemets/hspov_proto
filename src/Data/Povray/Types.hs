module Data.Povray.Types where

type Str = String -- may change to ByteString or Text

data Vector a = V a a a
type Vect = Vector Double

instance Functor Vector where
    fmap f (V p q r) = V (f p) (f q) (f r)

instance Applicative Vector where
    pure x = V x x x
    (V f0 f1 f2) <*>  (V x y z) = V (f0 x) (f1 y) (f2 z)

instance Num a => Num (Vector a) where
    v0 + v1 = (+) <$> v0 <*> v1
    negate = fmap negate
    v0 * v1 = (*) <$> v0 <*> v1
    abs = fmap abs
    fromInteger n = fromIntegral <$> V n n n
    signum = fmap signum
