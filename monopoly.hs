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
 cantidadDeDinero       = 500,
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

mapTacticaDeJuego :: Tactica -> Jugador -> Jugador
mapTacticaDeJuego tacticaNueva jugador = jugador {tacticaDeJuego = tacticaNueva}

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
pasarPorElBanco = mapTacticaDeJuego ("Comprador compulsivo") . mapCantidadDeDinero (+40)

enojarse :: Accion
enojarse = agregarAcciones gritar . mapCantidadDeDinero (+50)

gritar :: Accion
gritar = mapNombre ("AHHHH "++)

subastar :: Propiedad -> Accion
subastar propiedad jugador
 | tieneTacticasNecesarias jugador = agregarPropiedad propiedad (mapCantidadDeDinero (subtract (precioPropiedad propiedad)) jugador)
 | otherwise = id jugador

tieneTacticasNecesarias :: Jugador -> Bool
tieneTacticasNecesarias jugador = tacticaDeJuego jugador == "Accionista" || tacticaDeJuego jugador == "Oferente singular"

cobrarAlquileres :: Accion
cobrarAlquileres jugador = mapCantidadDeDinero (+precioTotalDePropiedadBarata jugador) (mapCantidadDeDinero (+precioTotalDePropiedadCaro jugador) jugador)

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata = (<150) . precioPropiedad

esPropiedadCaro :: Propiedad -> Bool
esPropiedadCaro = (>=150) . precioPropiedad

cantidadDePropiedadBarata :: Jugador -> Int
cantidadDePropiedadBarata = length . filter esPropiedadBarata . propiedadesCompradas

cantidadDePropiedadCaro :: Jugador -> Int
cantidadDePropiedadCaro = length . filter esPropiedadCaro . propiedadesCompradas

precioTotalDePropiedadBarata :: Jugador -> Int
precioTotalDePropiedadBarata = (*10) . cantidadDePropiedadBarata

precioTotalDePropiedadCaro :: Jugador -> Int
precioTotalDePropiedadCaro = (*20) . cantidadDePropiedadCaro

pagarAAccionistas :: Accion
pagarAAccionistas jugador 
 | esAccionista jugador = mapCantidadDeDinero (+200) jugador
 | otherwise = mapCantidadDeDinero (subtract 100) jugador

esAccionista :: Jugador -> Bool
esAccionista = (== "Accionista" ) . tacticaDeJuego 

hacerBerrinchePor :: Accion
hacerBerrinchePor = gritar . mapCantidadDeDinero (+10)

ultimaRonda :: Accion
ultimaRonda jugador = foldr ($) jugador $ accionesQueHicieron jugador  

juegoFinal :: Jugador -> Jugador -> String
juegoFinal jugador1 jugador2 
 | dineroDespuesDeUltimaRonda jugador1 > dineroDespuesDeUltimaRonda jugador2 = nombre jugador1
 | otherwise = nombre jugador2

dineroDespuesDeUltimaRonda :: Jugador -> Int
dineroDespuesDeUltimaRonda = cantidadDeDinero . ultimaRonda
