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
-- Dada una ciudad necesitamos saber cuáles son los tipos de unidades grosas. Una unidad es grosa si tiene más de 160 potencios de nivel de ataque.
unidadesGrosas:: Ciudad -> [String]
unidadesGrosas = map tipo.filter esUnidadGrosa.unidades

esUnidadGrosa:: Unidad ->Bool
esUnidadGrosa = tieneMasNivelDeAtaqueQue 160

tieneMasNivelDeAtaqueQue:: Potencio -> Unidad ->Bool
tieneMasNivelDeAtaqueQue potencios = (>potencios).nivelDeAtaque

-- Dada una ciudad queremos saber si tiene un ataque poderoso. Esto ocurre si las primeras 3 unidades de la ciudad tienen más de 100 potencios de nivel de ataque.
ataquePoderoso :: Ciudad -> Bool
ataquePoderoso = all(tieneMasNivelDeAtaqueQue 100). take 3.unidades

-- Dada una unidad queremos saber el nivel total que le aportan las herramientas. Esto es la sumatoria de nivel de potencios que aporta cada una.
poderTotalDeHerramientas :: Unidad -> Potencio 
poderTotalDeHerramientas = sum.map poder. herramientas 

-- Punto 2

-- Ahora necesitamos determinar el valor del poderOfensivo de una unidad. 
-- Esto se da con un cálculo que se define en base a una serie de condiciones. 

-- Si tiene más de 5 herramientas el poder ofensivo es 100 potencios sumado al nivel de ataque propio 
-- más el nivel que aporta cada herramienta. 

-- De lo contrario, si es del tipo "caballería" el cálculo es el doble de la sumatoria del nivel de ataque propio 
-- y el nivel que aporta cada herramienta. 

-- En caso contrario solamente es el nivel propio de ataque más el nivel que aporta cada herramienta.

poderOfensivo:: Unidad -> Potencio 
poderOfensivo guerrero 
 | ((>5).length.herramientas) guerrero = 100 + ataqueTotal guerrero 
 | ((=="Caballeria").tipo) guerrero = ataqueTotal guerrero * 2
 | otherwise = ataqueTotal guerrero

ataqueTotal :: Unidad -> Potencio
ataqueTotal guerrero = nivelDeAtaque guerrero + poderTotalDeHerramientas guerrero 

-- Punto 3

-- Queremos saber si un batallón sobrevive al ataque de otro. Esto ocurre cuando el batallón rival se queda sin unidades. 
-- Si por el contrario el batallón propio es el que se queda sin unidades, entonces no se cumple la condición. 
-- Los batallones se deben ir enfrentando en batallas individuales a las unidades en el orden que se encuentran. 
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

-- La muralla que dada la altura de la misma aumenta el nivel defensivo de la ciudad en el triple de su altura y 
-- le agrega el prefijo "La gran ciudad de" al nombre de la ciudad. 
-- Ejemplo: A Beijing que tiene 20 "defensios" de nivel defensivo le agregamos una muralla de 4 metros, esto incrementa 12 "defensios" su nivel
-- , de manera que pasa a tener 32 "defensios".
muralla :: Number -> SistemaDeDefensa
muralla altura ciudad = aumentarnivelDeDefensa (altura * 3) ciudad {
  nombre = "La gran " ++ nombre ciudad
}
-- Las torres de vigilancia que nos permiten preparar a la ciudad ante un posible ataque con antelación. Esto incrementa el nivel de defensa en 40 "defensios".
torresDeVigilancia:: SistemaDeDefensa
torresDeVigilancia = aumentarnivelDeDefensa 40

-- El centro de entrenamiento que incrementa un determinado valor el nivel de ataque propio que tiene cada unidad que posee su batallón de defensa. 
-- Además sube 10 "defensios" el nivel de defensa de la ciudad.
centroEntrenamiento :: Potencio -> SistemaDeDefensa
centroEntrenamiento incremento ciudad = aumentarnivelDeDefensa 10 ciudad{
  unidades = map (incrementarPoderAtaque incremento).unidades $ ciudad
}

incrementarPoderAtaque:: Potencio -> Unidad -> Unidad
incrementarPoderAtaque incremento unidad = unidad {nivelDeAtaque = nivelDeAtaque unidad + incremento}

-- Instalar bancos en las plazas. Esto no incrementa el nivel de defensa bajo ningún punto de vista.
instalarBancos :: SistemaDeDefensa
instalarBancos = id

-- Punto 5
-- odelar la función poderDefensivo que nos da el valor del nivel de defensa luego de activar todos los sistemas de defensa en la ciudad. 
-- Dar un ejemplo de cómo se invocará a la ciudad Persepolis con un nivel de defensa de 10, sin batallón de defensa pero con una muralla de 5 metros, 
-- un centro de entrenamiento de 15 "defensios" adicionales para las unidades del batallón y una torre de vigilancia.
poderDefensivo :: Ciudad -> Defensio 
poderDefensivo ciudad = nivelDeDefensa.foldr ($) ciudad . sistemasDeDefensa $ ciudad 

poderResultante:: Defensio
poderResultante = poderDefensivo $ Ciudad "Persepolis" 10 [] [muralla 5, centroEntrenamiento 15, torresDeVigilancia]


-- Punto 6
-- Saber si una ciudad sobrevive al ataque de un batallón. Esto ocurre con dos condiciones: el batallón de defensa sobrevive 
-- al ataque del batallón atacante o bien el poder defensivo de la ciudad es superior al poder total de ataque del batallón.
sobreviveCiudad :: Ciudad -> Batallon -> Bool 
sobreviveCiudad ciudad batallon = sobreviveElBatallon (unidades ciudad) batallon || poderDefensivo ciudad > poderAtaqueBatallon batallon

poderAtaqueBatallon :: Batallon -> Potencio
poderAtaqueBatallon = sum.map poderOfensivo

--Punto 7 
-- No podemos determinar los tipos de unidades grosas debido a que el algoritmo diverge, pero podemos determinar 
-- si tiene un ataque poderoso gracias a Lazy Evaluation que hace que el algoritmo converga a un valor. 

