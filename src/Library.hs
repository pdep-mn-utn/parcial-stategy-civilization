module Library where
import PdePreludat

data Unidad = Unidad {
  tipo :: String,
  nivelDeAtaque :: Number,
  herramientas :: [Herramienta]
} deriving (Show)

type Denominacion = String 
type Poder = Number
type Herramienta = (Denominacion, Poder)
denominacion = fst
poder = snd

data Ciudad = Ciudad { 
  nombre :: String,
  nivelDeDefensa :: Number,
  unidades :: [Unidad],
  sistemasDeDefensa :: [SistemaDeDefensa]
} deriving (Show)

type SistemaDeDefensa = Ciudad -> Ciudad


-- Puno 1
tiposGrosos:: Ciudad -> [String]
tiposGrosos = map tipo.filter ((>160).nivelDeAtaque).unidades

ataquePoderoso :: Ciudad -> Bool
ataquePoderoso = all((>100).nivelDeAtaque). take 3.unidades

poderTotalDeHerramientas :: Unidad -> Poder 
poderTotalDeHerramientas = sum.map poder. herramientas 

-- Punto 2

poderOfensivo:: Unidad -> Poder 
poderOfensivo guerrero 
 | ((>5).length.herramientas) guerrero = 100 + ataqueTotal guerrero 
 | ((=="caballeria").tipo) guerrero = ataqueTotal guerrero * 2
 | otherwise = ataqueTotal guerrero

ataqueTotal :: Unidad -> Poder
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
aumentarnivelDeDefensa :: Number -> SistemaDeDefensa
aumentarnivelDeDefensa cantidad ciudad = ciudad {nivelDeDefensa = nivelDeDefensa ciudad + cantidad}

muralla :: Number -> SistemaDeDefensa
muralla altura ciudad = aumentarnivelDeDefensa (altura * 3) ciudad {
  nombre = "La gran " ++ nombre ciudad
}

torresDeVigilancia:: SistemaDeDefensa
torresDeVigilancia = aumentarnivelDeDefensa 40

centroEntrenamiento :: Number -> SistemaDeDefensa
centroEntrenamiento incremento ciudad = aumentarnivelDeDefensa 10 ciudad{
  unidades = map (incrementarPoderAtaque incremento).unidades $ ciudad
}

incrementarPoderAtaque:: Number -> Unidad -> Unidad
incrementarPoderAtaque incremento unidad = unidad {nivelDeAtaque = nivelDeAtaque unidad + incremento}


-- Punto 5

poderDefensivo :: Ciudad -> Number 
poderDefensivo ciudad = nivelDeDefensa.foldr ($) ciudad $ sistemasDeDefensa ciudad 

poderResultante:: Number
poderResultante = poderDefensivo $ Ciudad "Persepolis" 10 [] [muralla 5, centroEntrenamiento 15, torresDeVigilancia]


-- Punto 6
sobreviveCiudad :: Ciudad -> Batallon -> Bool 
sobreviveCiudad ciudad batallon = sobreviveElBatallon (unidades ciudad) batallon || poderDefensivo ciudad > poderAtaqueBatallon batallon

poderAtaqueBatallon :: Batallon -> Number
poderAtaqueBatallon = sum.map poderOfensivo

--Punto 7 
-- No podemos determinar los tipos de unidades grosas debido a que el algoritmo diverge, pero podemos determinar 
-- si tiene un ataque poderoso gracias a Lazy Evaluation que hace que el algoritmo converga a un valor. 

