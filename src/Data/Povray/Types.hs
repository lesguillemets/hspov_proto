module Data.Povray.Types where

import Data.List
import Data.Monoid

import Data.Povray.Base

data Vector a = V a a a
type Vect = Vector Double

instance Functor Vector where
    fmap f (V p q r) = V (f p) (f q) (f r)

instance Applicative Vector where
    pure x = V x x x
    (V f0 f1 f2) <*>  (V x y z) = V (f0 x) (f1 y) (f2 z)

instance Foldable Vector where
    foldMap f (V x y z) = mconcat [f x, f y, f z]
-- |
-- >>> foldl (+) 0 (V 1 2 3)
-- 6
toList :: Vector a -> [a]
toList = foldr (:) []

instance Num a => Num (Vector a) where
    v0 + v1 = (+) <$> v0 <*> v1
    negate = fmap negate
    v0 * v1 = (*) <$> v0 <*> v1
    abs = fmap abs
    fromInteger n = fromIntegral <$> V n n n
    signum = fmap signum

instance (Show a) => Povray (Vector a) where
    toPov v = '<' : (intercalate ", " . map show . toList) v `mappend` ">"
-- |
-- >>> show (V 0 1 2)
-- "<0, 1, 2>"

instance Povray Double where toPov = show
