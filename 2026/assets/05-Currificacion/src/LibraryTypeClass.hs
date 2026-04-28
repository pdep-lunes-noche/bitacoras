module Library where
import PdePreludat

import Data.Char (toUpper, isUpper)

-------------------------------------------------
-- MODELO
-------------------------------------------------

type Nombre = String
type Habilidad = String
type Fuerza = Number

data Barbaro = Barbaro
  { nombre      :: Nombre
  , fuerza      :: Fuerza
  , habilidades :: [Habilidad]
  } deriving (Show, Eq)

class Objeto a where
  usar :: a -> Barbaro -> Barbaro

  repetirUso :: a -> Barbaro -> Barbaro
  repetirUso obj =
    usar obj . usar obj

data Espada = Espada { kilos :: Number }
  deriving (Show, Eq)

data Amuleto = Amuleto { habilidadOtorgada :: Habilidad }
  deriving (Show, Eq)

data Ardilla = Ardilla
  deriving (Show, Eq)

data Megafono = Megafono
  deriving (Show, Eq)

instance Objeto Espada where
  usar espada barb =
    barb {
      fuerza = fuerza barb + 2 * kilos espada
    }

instance Objeto Amuleto where
  usar amuleto barb =
    barb {
      habilidades =
        habilidadOtorgada amuleto :
        habilidades barb
    }

instance Objeto Ardilla where
  usar _ = id

instance Objeto Megafono where
  usar _ barb =
    barb {
      habilidades =
        [ map toUpper (concat (habilidades barb)) ]
    }

data Cuerda a b = Cuerda a b

instance (Objeto a, Objeto b) => Objeto (Cuerda a b) where
  usar (Cuerda obj1 obj2) = (.) (usar obj1) (usar obj2)

dave =
  Barbaro "Dave" 100
  ["tejer","Escribir Poesia Atroz"]

espadaPesada = Espada 10
ardilla = Ardilla
megafono = Megafono
megafonoBarbarico = Cuerda ardilla megafono

-- usar megafonoBarbarico dave