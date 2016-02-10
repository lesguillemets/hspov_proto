import Data.Povray
import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Camera
import Data.Povray.Colour
import Data.Povray.Light
import Data.Povray.Object
import Data.Povray.ObjectModifiers
import Data.Povray.Pigment
import Data.Povray.Material
import Data.Povray.Texture
import Data.Povray.Transformation

o :: Vect
o = V 0 0 0
v :: Vect
v = V 1 1 1
eColor :: Colour
eColor = RGB (V 0 0.8 1.5)
white = RGB (V 1 1 1)
black = RGB (V 0 0 0)

ePig :: Pigment
ePig = emptyPigment {_color = Just eColor}
eCheck :: Pigment
eCheck = emptyPigment {_named = Just (Checker (Just white) (Just black))}

eTexture :: Texture
eTexture = Texture (Just ePig) (Just (Phong 0.5))
box :: Object
box = Box o (V 1 1 1) (emptyModifier {_texture = Just eTexture})


eFloorTexture :: Texture
eFloorTexture = Texture (Just eCheck) (Just (Phong 0.1))
eFloor :: Object
eFloor = Plane (V 0 1 0) 0 (emptyModifier {_texture = Just eFloorTexture})

mGlass :: Material
mGlass = NamedMaterial "M_Glass"

glassSp :: Object
glassSp = Sphere (negate v) 2 (emptyModifier {_material=Just mGlass})

camera :: Camera
camera = Camera (V 2 9 10) o Nothing (Just (V 0 0.1 0))

spotLight :: LightKind
spotLight = Spotlight o (Just 18) (Just 20) Nothing
lights :: [Light]
lights = [
    Light (V 100 100 (-100)) (RGB (V 0.2 0.2 0.2)) Nothing,
    Light (V 0 10 (-120)) (RGB (V 0.3 0.3 0.3)) Nothing,
    Light (V 10 10 10) (RGB (V 0.8 1.5 0.8)) (Just spotLight)
    ]

main = do
    include "colors.inc"
    include "textures.inc"
    include "glass.inc"
    put camera
    put eFloor
    put box
    put glassSp
    mapM_ put lights
