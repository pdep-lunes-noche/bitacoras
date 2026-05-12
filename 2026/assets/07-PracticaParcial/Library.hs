module Library where
import PdePreludat

data Perro = Perro {
  nombre :: String,
  stamina :: Number,
  juguete :: Juguete,
  puedeJugar :: CriterioJuego -- Criterio
}

sultan = Perro{
  nombre = "sultan",
  stamina = 25,
  juguete = ("azul",False),
  puedeJugar = rechina.juguete
}

rechina :: Juguete -> Bool
rechina = snd 

type Juguete = (String, Bool)
type CriterioJuego = Perro -> Bool

aumentarStamina :: Number -> Perro -> Perro
aumentarStamina cantidad unPerro = unPerro{stamina = (+cantidad).stamina $ unPerro}

alimentarPerro :: Perro -> Perro
alimentarPerro = aumentarStamina 3

cambiarJuguete :: Juguete -> Perro -> Perro
cambiarJuguete unJuguete unPerro = unPerro{juguete = unJuguete }

modificarStamina :: (Number -> Number) -> Perro -> Perro
modificarStamina modificador unPerro = unPerro{stamina = modificador.stamina $ unPerro}

recibirJuguete :: Juguete -> Perro -> Perro
recibirJuguete unJuguete = modificarStamina (+3) . cambiarJuguete unJuguete

-- c) Hacer una funcion puedeJugarJuntos :: Perro -> Perro-> Bool
-- que se cumpla si mutuamente cumplen el criterio de juego del otro

puedenJugarJuntos :: Perro -> Perro -> Bool
puedenJugarJuntos perroA perroB = puedeJugar perroA perroB && puedeJugar perroB perroA

-- d) Hacer una funcion que vuelva mas exigente a un
-- perro para poder jugar con otro. Dado un nuevo criterio
-- de juego, se espera que el perro luego de volverse mas
-- exigente requiera que el otro perro cumpla no solo su
-- criterio previo, sino tambien el nuevo criterio
-- indicado. 

masExigente :: CriterioJuego -> Perro -> Perro
masExigente nuevoCriterio unPerro 
  = unPerro{puedeJugar = juntarCriterios nuevoCriterio (puedeJugar unPerro)}

-- Criterio = Perro -> Bool
juntarCriterios :: CriterioJuego -> CriterioJuego -> Perro -> Bool
juntarCriterios unCriterio otroCriterio unPerro 
 = (&&) (unCriterio unPerro) (otroCriterio unPerro)

type Tarea = Perro -> Perro
type Rutina = [Tarea]
realizarRutina :: Rutina -> Perro -> Perro

realizarRutina rutina perro = foldl (flip ($)) perro rutina
realizarRutina' :: Rutina -> Perro -> Perro
realizarRutina' rutina perro = foldl1 (.) rutina perro
realizarRutina'' :: Rutina -> Perro -> Perro
realizarRutina'' rutina perro = foldr ($) perro rutina
realizarRutina''' :: Rutina -> Perro -> Perro
realizarRutina''' rutina perro = foldl (\ unPerro tarea -> tarea unPerro) perro rutina

realizarRutina''' [] perro = perro

esRutinaExigente :: Rutina -> Perro -> Bool
esRutinaExigente rutina perro = ((stamina.realizarRutina rutina) $ perro) <= ((/2).stamina $ perro)


-- esRutinaExigente [comer, correr 10, saltar] sultan
compararJuguetes :: Juguete -> Juguete -> Bool

elegirUnJuguete :: [Juguete] -> Perro -> Perro
elegirUnJuguete (jugueteNuevo:juguetes) perro 
  | compararJuguetes jugueteNuevo juguete perro = cambiarJuguete jugueteNuevo perro
  | otherwise = elegirUnJuguete juguetes perro

elegirUnJuguete [] perro = perro

elegirElMEJORJuguete juguetes perro = foldr1 compararJuguetes juguetes (juguete perro)


perrosDeAcademia :: [Perro]
juntar5Amigos perros perro = take 5 . filter (puedeJugar perro) perros