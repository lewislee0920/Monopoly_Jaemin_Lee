import Text.Show.Functions

data Jugador = Jugador {
 nombre                :: String,
 cantidadDeDinero      :: Int,
 tacticaDeJuego        :: Tactica,
 propiedadesCompradas  :: [Propiedad],
 accionesQueHicieron   :: [Accion]
} deriving Show

data Propiedad = Propiedad {
 nombrePropiedad       :: String,
 precioPropiedad       :: Int
} deriving Show

type Accion = Jugador -> Jugador
type Tactica = String 

-- Ejemplos de Jugadores y Propiedades para pruebas --

carolina = Jugador {
 nombre                 = "Carolina",
 cantidadDeDinero       = 500,
 tacticaDeJuego         = "Accionista",
 propiedadesCompradas   = [casa, casa, casa, auto],
 accionesQueHicieron    = [pasarPorElBanco, pagarAAccionistas]
}

manuel = Jugador {
 nombre                 = "Manuel",
 cantidadDeDinero       = 450,
 tacticaDeJuego         = "Oferente singular",
 propiedadesCompradas   = [],
 accionesQueHicieron    = [pasarPorElBanco, enojarse]
}

casa = Propiedad {
    nombrePropiedad     = "Casa Gigante",
    precioPropiedad     = 500
}

auto = Propiedad {
    nombrePropiedad     = "Lamborgini",
    precioPropiedad     = 100
}

-- Funciones para modificar el Jugador --

mapNombre :: (String -> String) -> Jugador -> Jugador
mapNombre f jugador = jugador {nombre = f (nombre jugador)}

mapCantidadDeDinero :: (Int -> Int) -> Jugador -> Jugador
mapCantidadDeDinero f jugador = jugador { cantidadDeDinero = max 0 . f $ cantidadDeDinero jugador }

mapTacticaDeJuego :: (Tactica -> Tactica) -> Jugador -> Jugador
mapTacticaDeJuego f jugador = jugador {tacticaDeJuego = f (tacticaDeJuego jugador)}

mapAccionesQueHicieron :: ([Accion] -> [Accion]) -> Jugador -> Jugador 
mapAccionesQueHicieron f jugador = jugador { accionesQueHicieron = f $ accionesQueHicieron jugador}

agregarAcciones :: Accion -> Jugador -> Jugador 
agregarAcciones accion = mapAccionesQueHicieron (accion :)

mapPropiedad :: ([Propiedad] -> [Propiedad]) -> Jugador -> Jugador 
mapPropiedad f jugador = jugador { propiedadesCompradas = f $ propiedadesCompradas jugador}

agregarPropiedad :: Propiedad -> Jugador -> Jugador 
agregarPropiedad propiedad = mapPropiedad (propiedad :)

-- Funciones para Resolucion de Problemas --

pasarPorElBanco :: Accion
pasarPorElBanco = mapTacticaDeJuego (const "Comprador compulsivo") . mapCantidadDeDinero (+40)

enojarse :: Accion
enojarse = agregarAcciones gritar . mapCantidadDeDinero (+50)

gritar :: Accion
gritar = mapNombre ("AHHHH "++)

subastar :: Propiedad -> Accion
subastar propiedad jugador
 | tieneTacticasNecesarias (tacticaDeJuego jugador) = ganarPropiedad propiedad jugador
 | otherwise = id jugador

ganarPropiedad :: Propiedad -> Jugador -> Jugador
ganarPropiedad propiedad = agregarPropiedad propiedad . mapCantidadDeDinero (subtract (precioPropiedad propiedad))

tieneTacticasNecesarias :: String -> Bool
tieneTacticasNecesarias "Accionista" = True
tieneTacticasNecesarias "Oferente singular" = True
tieneTacticasNecesarias otros = False

cobrarAlquileres :: Accion
cobrarAlquileres jugador =  flip mapCantidadDeDinero jugador (+ ( sum $ map (precioAlquiler) (propiedadesCompradas jugador)))

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata = (<150) . precioPropiedad

precioAlquiler :: Propiedad -> Int
precioAlquiler propiedad
 | esPropiedadBarata propiedad = 10
 | otherwise = 20

pagarAAccionistas :: Accion
pagarAAccionistas jugador 
 | esAccionista jugador = mapCantidadDeDinero (+200) jugador
 | otherwise = mapCantidadDeDinero (subtract 100) jugador

esAccionista :: Jugador -> Bool
esAccionista = (== "Accionista" ) . tacticaDeJuego 

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad jugador 
 | not (puedeComprarLaPropiedad propiedad jugador) = hacerBerrinchePor propiedad . gritar . mapCantidadDeDinero (+10) $ jugador
 | otherwise = ganarPropiedad propiedad jugador

puedeComprarLaPropiedad :: Propiedad -> Jugador -> Bool
puedeComprarLaPropiedad propiedad = (>= (precioPropiedad propiedad)) . cantidadDeDinero 

ultimaRonda :: Jugador -> (Accion)
ultimaRonda jugador = foldl1 (.) $ accionesQueHicieron jugador 


juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal jugador1 jugador2 
 | dineroDespuesDeUltimaRonda jugador1 > dineroDespuesDeUltimaRonda jugador2 = terminarRonda jugador1
 | otherwise = terminarRonda jugador2

dineroDespuesDeUltimaRonda :: Jugador -> Int
dineroDespuesDeUltimaRonda = cantidadDeDinero . terminarRonda

terminarRonda :: Accion 
terminarRonda jugador = (ultimaRonda jugador) jugador
