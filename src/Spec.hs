module Spec where
import Library
import PdePreludat
import Test.Hspec

unidadTanque = Unidad "Tanque" 105 [("Tanqueta",20)] 
unidad = Unidad "Caballeria" 101 [("fusil",10),("pistola",20),("cuchillo",20),("remo",20),("chancleta",20),("ametralladora",20)] 
unidadGrosa = Unidad "Grosa" 200 [] 
moto = Unidad "Moto" 50 []
caballeria = Unidad "Caballeria" 200 [] 

batallonPersepolis = [caballeria, moto]
batallonUr = [unidad,unidadTanque,unidadGrosa]

persepolis = Ciudad "Persepolis" 100 [unidad, unidadGrosa, unidadTanque] []
ur = Ciudad "Ur" 1000 [unidad, moto, unidadGrosa, unidadTanque] []
beijing = Ciudad "Beijing" 20 [unidad] [muralla 4,torresDeVigilancia]

correrTests :: IO ()
correrTests = hspec $ do
  describe "Punto 1" $ do
    it "Dada una unidad con suficiente poder de ataque, entonces es grosa" $ do
      unidadGrosa `shouldSatisfy` esUnidadGrosa
    it "Dada una unidad sin suficiente poder de ataque, entonces no es grosa" $ do
      unidad `shouldNotSatisfy` esUnidadGrosa
    it "Dada una ciudad, se pueden obtener sus unidades grosas" $ do
      unidadesGrosas persepolis `shouldContain` ["Grosa"]
      (length.unidadesGrosas) persepolis `shouldBe` 1
    it "Dada una ciudad con sus primeras n unidades con suficiente poder, entonces tiene ataque poderoso" $ do
      persepolis `shouldSatisfy` ataquePoderoso
    it "Dada una ciudad con sus primeras n unidades con suficiente poder, entonces tiene ataque poderoso" $ do
      ur `shouldNotSatisfy` ataquePoderoso
    it "Dada una unidad con herramientas, entonces se obtiene el poder total de sus herramientas" $ do
      poderTotalDeHerramientas unidadTanque `shouldBe` 20
    it "Dada una unidad sin herramientas, entonces el poder total de sus herramientas es cero" $ do
      poderTotalDeHerramientas moto `shouldBe` 0

  describe "Punto 2" $ do 
    it "Dada una unidad, entoces su poder de ataque total se calcula como el propio más el de sus herramientas" $ do
      ataqueTotal unidadTanque `shouldBe` 125
    it "Dada una unidad con suficientes herramientas, entonces su poder ofensivo es el poder base + su ataque total" $ do 
      poderOfensivo unidad `shouldBe` 311
    it "Dada una unidad sin suficientes herramientas que es de un tipo específico, entonces su poder ofensivo es el ataque total por un coeficiente" $ do 
      poderOfensivo caballeria `shouldBe` 400
    it "Dada una unidad sin suficientes herramientas ni del tipo específico, entonces su poder ofensivo es su poder de ataque total" $ do
      poderOfensivo moto `shouldBe` 50

  describe "Punto 3" $ do 
    it "Dada dos batallones, sobrevive el primero si tiene el poder ofensivo suficiente para vencer al otro" $ do
      batallonUr `shouldSatisfy` sobreviveElBatallon batallonPersepolis
    it "Dada dos batallones, no sobrevive el primero si no cuenta con el poder ofensivo suficiente para vencer al otro" $ do
      batallonPersepolis `shouldNotSatisfy` sobreviveElBatallon batallonUr

  describe "Punto 4" $ do 
    it "Dada un nivel de defensios y una ciudad, entonces aumentarNivelDeDefensa le agrega dicho valor" $ do
      (nivelDeDefensa.aumentarnivelDeDefensa 10) persepolis `shouldBe` 110
    it "Dada una muralla con una cierta altura y una ciudad, entonces muralla le agrega dicho valor por un factor y le antepone un prefijo al nombre" $ do
      (nivelDeDefensa.muralla 4) beijing `shouldBe` 32
      (nombre.muralla 4) beijing `shouldBe` "La gran Beijing"
    it "Dada una torre de vigilacia y una ciudad, entonces aumenta su nivel de defensios en un valor fijo" $ do
      (nivelDeDefensa.torresDeVigilancia) beijing `shouldBe` 60
    it "Dado un centro de entrenamiento y una ciudad, entonces aumenta su nivel de defensios en un valor fijo" $ do
      (nivelDeDefensa.centroEntrenamiento 20) beijing `shouldBe` 30
    it "Dado un centro de entrenamiento y una ciudad, entonces aumenta el nivel de cada integrante del batallón de potencios en un valor determinado" $ do
      (nivelDeAtaque.head.unidades.centroEntrenamiento 10) beijing `shouldBe` 111
  
  describe "Punto 5" $ do 
    it "Dada una ciudad con sistemas de defensa, entonces el poder defensivo es su nivel de defensa luego de plaicar los sistemas en la misma" $ do
      poderDefensivo beijing `shouldBe` 72
    it "Dada una ciudad sin sistemas de defensa, entonces el poder defensivo es su nivel de defensa" $ do
      poderDefensivo persepolis `shouldBe` 100
  
  describe "Punto 6" $ do
    it "Dado un batallón, poderAtaqueBatallon obtiene el total del mismo" $ do
      poderAtaqueBatallon batallonUr `shouldBe` 636
    it "Dada una ciudad con un battallón de suficiente energía para vencer al atacante, entonces sobrevive la ciudad" $ do
      batallonUr `shouldSatisfy` sobreviveCiudad persepolis {unidades = batallonPersepolis}
    it "Dada una ciudad con un battallón sin suficiente energía para vencer al atacante perco con suficiente poder de defensa, entonces sobrevive la ciudad" $ do
      batallonPersepolis `shouldSatisfy` sobreviveCiudad ur 
    it "Dada una ciudad con un battallón sin suficiente energía para vencer al atacante ni suficiente poder de defensa, entonces no sobrevive la ciudad" $ do
      batallonUr `shouldNotSatisfy` sobreviveCiudad persepolis 