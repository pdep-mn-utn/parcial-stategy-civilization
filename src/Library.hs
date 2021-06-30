module Library where
import PdePreludat

type Potencio = Number
type Defensio = Number

data Unidad = Unidad {
  tipo :: String,
  nivelDeAtaque :: Potencio,
  herramientas :: [Herramienta]
} deriving (Show)

type Denominacion = String 
type Herramienta = (Denominacion, Potencio)
denominacion = fst
poder = snd

data Ciudad = Ciudad { 
  nombre :: String,
  nivelDeDefensa :: Defensio,
  unidades :: [Unidad],
  sistemasDeDefensa :: [SistemaDeDefensa]
} deriving (Show)

type SistemaDeDefensa = Ciudad -> Ciudad


-- Puno 1
unidadesGrosas:: Ciudad -> [String]
unidadesGrosas = map tipo.filter ((>160).nivelDeAtaque).unidades

ataquePoderoso :: Ciudad -> Bool
ataquePoderoso = all((>100).nivelDeAtaque). take 3.unidades

poderTotalDeHerramientas :: Unidad -> Potencio 
poderTotalDeHerramientas = sum.map poder. herramientas 

-- Punto 2

poderOfensivo:: Unidad -> Potencio 
poderOfensivo guerrero 
 | ((>5).length.herramientas) guerrero = 100 + ataqueTotal guerrero 
 | ((=="caballeria").tipo) guerrero = ataqueTotal guerrero * 2
 | otherwise = ataqueTotal guerrero

ataqueTotal :: Unidad -> Potencio
ataqueTotal guerrero = nivelDeAtaque guerrero + poderTotalDeHerramientas guerrero 

-- Punto 3
type Batallon = [Unidad]

sobreviveElBatallon :: Batallon -> Batallon -> Bool
sobreviveElBatallon _ [] = True
sobreviveElBatallon [] _ = False
sobreviveElBatallon (defensor:defensores) (atacante:atacantes)
 | poderOfensivo defensor > poderOfensivo atacante = sobreviveElBatallon (defensor:defensores) atacantes
 | otherwise = sobreviveElBatallon defensores (atacante:atacantes)

-- Punto 4
aumentarnivelDeDefensa :: Defensio -> SistemaDeDefensa
aumentarnivelDeDefensa cantidad ciudad = ciudad {nivelDeDefensa = nivelDeDefensa ciudad + cantidad}

muralla :: Defensio -> SistemaDeDefensa
muralla altura ciudad = aumentarnivelDeDefensa (altura * 3) ciudad {
  nombre = "La gran " ++ nombre ciudad
}

torresDeVigilancia:: SistemaDeDefensa
torresDeVigilancia = aumentarnivelDeDefensa 40

centroEntrenamiento :: Potencio -> SistemaDeDefensa
centroEntrenamiento incremento ciudad = aumentarnivelDeDefensa 10 ciudad{
  unidades = map (incrementarPoderAtaque incremento).unidades $ ciudad
}

incrementarPoderAtaque:: Potencio -> Unidad -> Unidad
incrementarPoderAtaque incremento unidad = unidad {nivelDeAtaque = nivelDeAtaque unidad + incremento}


-- Punto 5

poderDefensivo :: Ciudad -> Defensio 
poderDefensivo ciudad = nivelDeDefensa.foldr ($) ciudad $ sistemasDeDefensa ciudad 

poderResultante:: Defensio
poderResultante = poderDefensivo $ Ciudad "Persepolis" 10 [] [muralla 5, centroEntrenamiento 15, torresDeVigilancia]


-- Punto 6
sobreviveCiudad :: Ciudad -> Batallon -> Bool 
sobreviveCiudad ciudad batallon = sobreviveElBatallon (unidades ciudad) batallon || poderDefensivo ciudad > poderAtaqueBatallon batallon

poderAtaqueBatallon :: Batallon -> Potencio
poderAtaqueBatallon = sum.map poderOfensivo

--Punto 7 
-- No podemos determinar los tipos de unidades grosas debido a que el algoritmo diverge, pero podemos determinar 
-- si tiene un ataque poderoso gracias a Lazy Evaluation que hace que el algoritmo converga a un valor. 

