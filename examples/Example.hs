import Data.Povray
import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Objects
import Data.Povray.ObjectModifiers
import Data.Povray.Texture
import Data.Povray.Transformation

main = do
    include "colors.inc"
    putStrLn . toPov $ Box (V 0 2 3) (V 3 5 10)  $
                    OModify Nothing (Just $ Translate (V 0 0 2)) (Just "COMMO")
