{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import GHC.Event.Windows (HandleData(_handleCallback))

-- /////////////////////////////////////////////////////

edad (_ , numero) = numero

type Persona = (String, Number)
laura :: Persona
laura = ("Laura", 22)

-- /////////////////////////////////////////////////////
doble :: Number -> Number
doble numero = numero + numero

aproboAlumno :: Number->Bool
aproboAlumno nota = nota >= 6

edadSeniorBurns = 120

segundoNombreDeHomero = "Jay"

pesosADolares :: Number->Number->Number
pesosADolares pesos precioDolar = pesos / precioDolar

numeroMayorEntre :: Number->Number->Number
numeroMayorEntre x y
 | x > y = x
 | otherwise = y

puedoAvanzar :: String -> Bool
puedoAvanzar color = color == "verde"

cuadruple :: Number->Number
cuadruple numero = (doble . doble) numero

nombrePar :: String->Bool
nombrePar nombre = (even . length) nombre

mayorDeEdad :: Number->Bool
mayorDeEdad edad = edad > 18

esMayorDeEdad :: (String , Number)->Bool
esMayorDeEdad persona = (mayorDeEdad . edad) persona

esMayorDeEdad_ :: Persona->Bool
esMayorDeEdad_ persona = (mayorDeEdad . edad) persona

siguiente :: Number->Number
siguiente numero = numero + 1

-- EJERCICIO DE COSTOS -- 

costoEstacionamiento :: Number->Number
costoEstacionamiento horas
 | horas <= 2 = 100
 | otherwise = horas * 50

costoEstacionamientoV2 hora = ((* 50) . max 2) hora

-- FIN DE EJERCICIO DE COSTOS -- 

data Color = Blanco | Azul | Rojo | Amarillo

costoPorLitro :: Color -> Number
costoPorLitro Blanco   = 500
costoPorLitro Azul     = 600
costoPorLitro Amarillo = 400
costoPorLitro _        = 400

valorMaximo :: (Number, Number) -> Number
valorMaximo (numero1, numero2) = max numero1 numero2

type Conjunto = (Number, Number)
valorMinimo :: Conjunto -> Number
valorMinimo (numero1, numero2) = min numero1 numero2

data PersonaV1 = PersonaV1 String Number
nombre (PersonaV1 _nombre _edad) = _nombre
age   (PersonaV1 _nombre _edad) = _edad 

uno = PersonaV1 -- Se puede pasar propiedades asi -- 

esMayorEdad :: PersonaV1 -> Bool
esMayorEdad = mayorDeEdad . age

data Persona_ = Persona_ {
    nombre_          :: String,
    edad_            :: Number,
    domicilio_       :: String,
    telefono_        :: String,
    fechaNacimiento_ :: (Number, Number, Number),
    buenaPersona_    :: Bool,
    plata_           :: Number
}deriving(Show)

juan = Persona_ {
    nombre_          = "Juan", 
    edad_            = 29,
    domicilio_       = "Ayacucho 554",
    telefono_        = "42979100",
    fechaNacimiento_ = (17, 7, 1988),
    buenaPersona_    = True, 
    plata_           = 30
} 