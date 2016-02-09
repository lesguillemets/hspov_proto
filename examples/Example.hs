import Data.Povray
import Data.Povray.Base
import Data.Povray.Types
import Data.Povray.Camera
import Data.Povray.Colour
import Data.Povray.Light
import Data.Povray.Objects
import Data.Povray.ObjectModifiers
import Data.Povray.Pigment
import Data.Povray.Material
import Data.Povray.Texture
import Data.Povray.Transformation

o :: Vect
o = V 0 0 0
ecolor :: Colour
ecolor = RGB (V 0 0.8 1.5)
white = RGB (V 1 1 1)
black = RGB (V 0 0 0)

epig :: Pigment
epig = emptyPigment {_color = Just ecolor}
eglass :: Pigment
eglass = emptyPigment {_named = Just (Named "M_Glass3")}
echeck :: Pigment
echeck = emptyPigment {_named = Just (Checker (Just white) (Just black))}

etexture :: Texture
etexture = Texture (Just epig) (Just (Phong 0.5))
box :: Object
box = Box o (V 1 1 1) (emptyModifier {_texture = Just etexture})


efloorTexture :: Texture
efloorTexture = Texture (Just echeck) (Just (Phong 0.1))
efloor :: Object
efloor = Plane (V 0 1 0) 0 (emptyModifier {_texture = Just efloorTexture})

mglass :: Material
mglass = NamedMaterial "M_Glass"

glassSp :: Object
glassSp = Sphere (V (-1) (-1) (-1)) 2 (emptyModifier {_material=Just mglass})

camera :: Camera
camera = Camera (V 2 9 10) o

lights :: [Light]
lights = [
    Light (V 100 100 (-100)) (RGB (V 1.2 1.2 1.2)),
    Light (V 0 10 (-120)) (RGB (V 0.3 0.3 0.3))
    ]
          
main = do
    include "colors.inc"
    include "textures.inc"
    include "glass.inc"
    put camera
    put efloor
    put box
    put glassSp
    mapM_ put lights
