module Data.Povray.Base where
import Data.Monoid

type Str = String -- may change to ByteString or Text

join :: [Str] -> Str
join = unlines . filter (not . null)

class Povray a where
    toPov :: a -> Str

formComment :: Str -> Str
formComment c = let cs = lines c in
                if length cs == 1 then "// " `mappend` c
                                  else "/*\n" `mappend` c `mappend` "\n*/"
