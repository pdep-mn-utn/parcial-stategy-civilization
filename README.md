# Strategy Civilization
[![build](https://github.com/pdep-mn-utn/parcial-stategy-civilization/actions/workflows/build.yml/badge.svg)](https://github.com/pdep-mn-utn/parcial-stategy-civilization/actions/workflows/build.yml)
 
¡Es hora de jugar! vamos a realizar el modelado de un juego de estrategia para divertirnos con amigos. 

## Descripción del dominio

En nuestro juego vamos a tener diferentes personajes que nos van a permitir atacar a diferentes objetivos que los vamos a denominar **unidades**. Las unidades tienen un tipo que las agrupa (por ejemplo arquero, caballería, catapulta, etc), un nivel de ataque y una serie de herramientas, como por ejemplo tenemos un hacha que aporta 10 potencios de ataque o bien podría ser una espada que aporta 15 potencios de ataque. (El potencio es una unidad de ataque que lo utilizamos para que no se confunda con el concepto **unidad** previamente descripto)

Además tenemos ciudades. Cada ciudad tiene un nivel propio de defensa y un batallón que envía a la batalla en caso de ataque. En el juego tenemos la posibilidad de poder agregar sistemas de defensa que al activarlo le dan ciertas propiedades a la misma. Vamos a detallar este punto más adelante.

---------------
## Punto 1 (3 puntos)

Modelar las abstracciones necesarias para cumplir con la descripción del dominio y saber que: 

- Dada una ciudad necesitamos saber cuáles son los tipos de unidades grosas. Una unidad es grosa si tiene más de 160 potencios de nivel de ataque. 
- Dada una ciudad queremos saber si tiene un ataque poderoso. Esto ocurre si las primeras 3 unidades de la ciudad tienen más de 100 potencios de nivel de ataque. 
- Dada una unidad queremos saber el nivel total que le aportan las herramientas. Esto es la sumatoria de nivel de potencios que aporta cada una.

> Este punto debe resolverse utilizando **funciones de orden superior**.
---------------
## Punto 2 (2 puntos)

Ahora necesitamos determinar el valor del **poderOfensivo** de una unidad. Esto se da con un cálculo que se define en base a una serie de condiciones. Si tiene más de 5 herramientas el poder ofensivo es 100 potencios sumado al nivel de ataque propio más el nivel que aporta cada herramienta. De lo contrario, si es del tipo "caballería" el cálculo es el doble de la sumatoria del nivel de ataque propio y el nivel que aporta cada herramienta. En caso contrario solamente es el nivel propio de ataque más el nivel que aporta cada herramienta. 

> En este punto deben evitar lógica y código repetido.
---------------
## Punto 3 (2 puntos)

Queremos saber si un batallón sobrevive al ataque de otro. Esto ocurre cuando el batallón rival se queda sin unidades. Si por el contrario el batallón propio es el que se queda sin unidades, entonces no se cumple la condición. Los batallones se deben ir enfrentando en batallas individuales a las unidades en el orden que se encuentran. Por ejemplo, la primera unidad de ambos bandos se enfrentan comparando su *poder ofensivo*. Quien supera en valor al contrincante es el sobreviviente y será quien siga para la próxima ronda.

> Resolver el punto utilizando **recursividad**. No puede utilizar recursividad en ninguna otro punto de su solución.

---------------
## Punto 4 (3 puntos)

Como habíamos comentado con anterioridad, las ciudades cuentan con diversos sistemas de defensa que se pueden ir agregando de forma dinámica. Además a futuro podemos implementar nuevos sistemas de defensa para complejizar el juego. Los sistemas de defensa que tenemos son:

- La muralla que dada la altura de la misma aumenta el nivel defensivo de la ciudad en el triple de su altura y le agrega el prefijo "La gran ciudad de" al nombre de la ciudad. Ejemplo: A Beijing que tiene 20 "defensios" de nivel defensivo le agregamos una muralla de 4 metros, esto incrementa 12 "defensios" su nivel, de manera que pasa a tener 32 "defensios".
- Las torres de vigilancia que nos permiten preparar a la ciudad ante un posible ataque con antelación. Esto incrementa el nivel de defensa en 40 "defensios". 
- El centro de entrenamiento que incrementa un determinado valor el nivel de ataque propio que tiene cada unidad que posee su batallón de defensa. Además sube 10 "defensios" el nivel de defensa de la ciudad. 
- Instalar bancos en las plazas. Esto no incrementa el nivel de defensa bajo ningún punto de vista.

> En este punto deben evitar lógica y código repetido.

---------------
## Punto 5 (3 puntos)

Modelar la función poderDefensivo que nos da el valor del nivel de defensa luego de activar todos los sistemas de defensa en la ciudad. Dar un ejemplo de cómo se invocará a la ciudad *Persepolis* con un nivel de defensa de 10, sin batallón de defensa pero con una muralla de 5 metros, un centro de entrenamiento de 15 "defensios" adicionales para las unidades del batallón y una torre de vigilancia.

---------------
## Punto 6 (1 punto)

Saber si una ciudad sobrevive al ataque de un batallón. Esto ocurre con dos condiciones: el batallón de defensa sobrevive al ataque del batallón atacante o bien el poder defensivo de la ciudad es superior al poder total de ataque del batallón.

---------------
## Punto 7 (1 puntos)

Si una ciudad tiene un batallón de infinitas unidades, que ocurre en el caso de solicitar los tipos de unidades grosas que posee y saber si tiene un ataque poderoso. Justifique su respuesta en base a los contenidos teóricos vistos en clase. 

# Puntajes

Puntos | Nota
------ | -----
15 | 10
14 | 9
12 - 13 | 8
10 - 11 | 7
9 | 6
8 - 9 | Revisión
< 8 | Desaprobado

