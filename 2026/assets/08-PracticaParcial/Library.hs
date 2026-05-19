module Library where
import PdePreludat

type Nombre = String
type Fuerza = Number
type Frase = String


data Personaje = Personaje {
  nombre :: Nombre,
  tienePoderes :: Bool,
  fuerza :: Fuerza,
  frasesSabias :: [Frase]
} deriving (Show,Eq)

finn = Personaje "Finn" False 300 []
jake = Personaje "Jake" True 1500 ["Mientras conozca la forma de mi alma,estaré bien", "Ser un poco malo en algo es el comienzo de ser un poco bueno en algo", "Si consigues todo lo que quieres en el momento ¿qué sentido tiene vivir?"]

type Mision = Personaje -> Bool

puedeHacerMision :: Mision -> Personaje -> Bool
puedeHacerMision mision personaje  =  cumpleCaracteristicaMisiones personaje && mision personaje


--cumplirMision' :: Mision -> Personaje -> Bool
--cumplirMision' mision personaje = liftA2 (&&) cumpleCaracteristicaMisiones mision personaje

--        Param 1   |  Param 2   | Param 3 | P4 | Resultado
--liftA2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
--liftA2 func2Params funcA1Param funcionB1Param parametro = func2Params (funcA1Param parametro) (funcionB1Param parametro)


cumpleCaracteristicaMisiones :: Personaje -> Bool
cumpleCaracteristicaMisiones = cumpleAlguna criteriosDeMisiones

cumpleAlguna :: [Personaje -> Bool] -> Personaje -> Bool
cumpleAlguna criterios personaje = any ($ personaje) criterios

type Caracteristica = Personaje -> Bool

criteriosDeMisiones :: [Caracteristica]
criteriosDeMisiones = [esFuerte, tienePoderes,esFinn]

esFuerte :: Caracteristica
esFuerte = (>500).fuerza

esFinn :: Caracteristica
esFinn = seLlama "Finn"

volverseReaggetonero :: Mision
volverseReaggetonero = seLlama "Gunther"

seLlama :: Nombre -> Personaje -> Bool
seLlama unNombre = (==unNombre).nombre

-- b) 

sandwichPerfecto :: Mision
sandwichPerfecto personaje = esSabio personaje && tienePoderes personaje

esSabio :: Personaje -> Bool
esSabio = (>=3).length.frasesSabias


-- c)

jugarConBMO :: Mision 
jugarConBMO  = cumpleAlguna [esFinn, seLlama "Jake", poderMayorA 5000]

poderMayorA :: Number -> Personaje -> Bool
poderMayorA cantidad = (>cantidad).poder

poder :: Personaje -> Number
poder personaje = (* multiplicadorPersonaje personaje).fuerza $ personaje

multiplicadorPersonaje :: Personaje -> Number
multiplicadorPersonaje personaje
  | tienePoderes personaje = multiplicadorConPoderes -- 10
  | otherwise = multiplicadorSinPoderes -- 5


multiplicadorPersonaje' :: Personaje -> Number
multiplicadorPersonaje' (Personaje _ True _ _) = 10
multiplicadorPersonaje' (Personaje _ False _ _) = 5



multiplicadorConPoderes :: Number
multiplicadorConPoderes = 10
multiplicadorSinPoderes :: Number
multiplicadorSinPoderes = 5

-- d)

esQueNoSoyMuyListo :: Mision
esQueNoSoyMuyListo personaje = (not.esSabio) personaje && (not.tienePoderes) personaje


-- Mejoras

type Mejora = Personaje -> Personaje


espadaDePasto :: Mejora
espadaDePasto personaje
  | esFinn personaje = darPoderes personaje
  | otherwise = personaje

darPoderes :: Personaje -> Personaje
darPoderes personaje = personaje{tienePoderes = True}

modificarFuerza :: (Fuerza -> Fuerza) -> Personaje -> Personaje
modificarFuerza modificador personaje = personaje{fuerza = modificador . fuerza $ personaje}

porFeoVendeBarato :: Mejora
porFeoVendeBarato = darPoderes . modificarFuerza (*2)

coronaHelada :: Mejora 
coronaHelada = darPoderes . cambiarNombre "Rey Helado" . eliminarFrases

cambiarNombre :: Nombre -> Personaje -> Personaje
cambiarNombre unnombre personaje = personaje{nombre = unnombre}

eliminarFrases :: Personaje -> Personaje
eliminarFrases personaje = personaje{frasesSabias = []}

-- 4) Grupos de Personajes.

nombresDeCompletadores :: Mision -> [Personaje] -> [Nombre]
nombresDeCompletadores mision = map nombre . filter (puedeHacerMision mision)

poderTotalGrupal :: [Personaje] -> Number
poderTotalGrupal personajes = (* length personajes).sum.map poder $ personajes


aplicarMejoras' :: [Mejora] -> Personaje -> Personaje
aplicarMejoras' mejoras personaje = foldl (flip ($)) personaje mejoras
aplicarMejoras'V2 :: [Mejora] -> Personaje -> Personaje
aplicarMejoras'V2 mejoras personaje = foldl (\ personaje mejora -> mejora personaje ) personaje mejoras
aplicarMejoras'' :: [Mejora] -> Personaje -> Personaje
aplicarMejoras'' mejoras personaje = foldr ($) personaje mejoras


aplicarMejoras :: [Mejora] -> Personaje -> Personaje
aplicarMejoras = foldr1 (.)

primerosQuePuedenCumplirMision :: Mision -> [Personaje] -> Number
primerosQuePuedenCumplirMision _ [] = 0

primerosQuePuedenCumplirMision mision (personaje:personajes)
  | puedeHacerMision mision personaje = 1 + primerosQuePuedenCumplirMision mision personajes
  | otherwise = 0

-- primerosQuePuedenCumplirMision sandwichPerfecto [jake,finn, pepe, moni]

-- 1 + primerosQuePuedenCumplirMision sandwichPerfecto [finn, pepe, moni]
-- 1 + 1 + primerosQuePuedenCumplirMision sandwichPerfecto [pepe, moni]
-- 1 + 1 + 0 -> 2


-- 1 + 1 + 1 + 1 + primerosQuePuedenCumplirMision sandwichPerfecto []
-- 1 + 1 + 1 + 1 + 0 -> 4