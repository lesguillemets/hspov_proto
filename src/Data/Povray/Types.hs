module Data.Povray.Types where

import Data.List

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
    fromInteger = pure . fromIntegral
    signum = fmap signum

instance (Show a) => Povray (Vector a) where
    toPov v = '<' : (intercalate ", " . map show . toList) v `mappend` ">"
-- |
-- >>> toPov (V 0 1 2)
-- "<0, 1, 2>"

data Vector4 a = V4 a a a a
type Vect4 = Vector Double

instance Functor Vector4 where
    fmap f (V4 p q r s) = V4 (f p) (f q) (f r) (f s)

instance Applicative Vector4 where
    pure x = V4 x x x x
    (V4 f0 f1 f2 f3) <*>  (V4 x y z w) = V4 (f0 x) (f1 y) (f2 z) (f3 w)

instance Foldable Vector4 where
    foldMap f (V4 x y z w) = mconcat [f x, f y, f z, f w]

instance Num a => Num (Vector4 a) where
    v0 + v1 = (+) <$> v0 <*> v1
    negate = fmap negate
    v0 * v1 = (*) <$> v0 <*> v1
    abs = fmap abs
    fromInteger = pure . fromIntegral
    signum = fmap signum

instance (Show a) => Povray (Vector4 a) where
    toPov v = '<' : (intercalate ", " . map show . foldr (:) []) v `mappend` ">"


data Vector5 a = V5 a a a a a
type Vect5 = Vector Double

instance Functor Vector5 where
    fmap f (V5 p q r s t) = V5 (f p) (f q) (f r) (f s) (f t)

instance Applicative Vector5 where
    pure x = V5 x x x x x
    (V5 f0 f1 f2 f3 f4) <*>  (V5 x y z w v)
        = V5 (f0 x) (f1 y) (f2 z) (f3 w) (f4 v)

instance Foldable Vector5 where
    foldMap f (V5 x y z w v) = mconcat [f x, f y, f z, f w, f v]

instance Num a => Num (Vector5 a) where
    v0 + v1 = (+) <$> v0 <*> v1
    negate = fmap negate
    v0 * v1 = (*) <$> v0 <*> v1
    abs = fmap abs
    fromInteger = pure . fromIntegral
    signum = fmap signum

instance (Show a) => Povray (Vector5 a) where
    toPov v = '<' : (intercalate ", " . map show . foldr (:) []) v `mappend` ">"
