{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module EjericiosPractica where
import PdePreludat
import GHC.Event.Windows (HandleData(_handleCallback))
import GHC.Num (Num)
import Library (siguiente)

esMultiploDe3:: Number->Bool
esMultiploDe3 numero = mod numero 3 == 0

esMultiploDeDos:: Number->Number->Bool
esMultiploDeDos numero1 numero2 = mod numero2 numero1 == 0

cubo:: Number->Number
cubo numero = numero^3

area:: Number->Number->Number
area base altura = base * altura

esBisiesto:: Number->Bool
esBisiesto anio = mod anio 400 == 0 || (mod anio 4 == 0) && not (mod anio 100 == 0)

esBisiestoV2:: Number->Bool
esBisiestoV2 anio = esMultiploDeDos 400 anio || esMultiploDeDos 4 anio && not (esMultiploDeDos 100 anio)

dispersion :: Number->Number->Number->Number
dispersion valor1 valor2 valor3 = (max valor1 . max valor2) valor3

diasParejos :: Number->Number->Number->Bool
diasParejos valor1 valor2 valor3 = dispersion valor1 valor2 valor3 < 30

diasLocos :: Number->Number->Number->Bool
diasLocos valor1 valor2 valor3 = dispersion valor1 valor2 valor3 > 30

diasNormales :: Number->Number->Number->Bool
diasNormales valor1 valor2 valor3 = not (diasLocos valor1 valor2 valor3) && not (diasParejos valor1 valor2 valor3)

type Peso   = Number
type Altura = Number
pesoPino :: Altura->Peso
pesoPino altura
 | altura <= 300 = 3 * altura
 | altura > 300  = 900 + 2 * (altura - 300) 

pesoUtil :: Peso->Bool
pesoUtil peso = peso > 400 && peso < 1000

sirvePino :: Altura->Bool
sirvePino altura = (pesoUtil . pesoPino) altura

-- EJERCICIOS DE APLICACION -- 

mitad :: Number->Number
mitad numero = (/2) numero

inversa :: Number->Number
inversa numero = 1/numero

laInversaMayorA5 :: Number->Bool
laInversaMayorA5 numero = ((> 5) . inversa) numero

-- EJERCICIOS DE TUPLAS -- 

primeroDe3(elemento, _ , _) = elemento
segundoDe3(_, elemento , _) = elemento
terceroDe3(_, _ , elemento) = elemento

aplicar :: (Number, Number)->(Number, Number)
aplicar (numero1, numero2) = ((*2) numero1, (*3) numero2) 
--aplicarV2 :: (Number->Number, Number->Number)->Number->(Number,Number)
--aplicarV2 funcion funcion1 numero = (funcion, funcion1) numero

cuentaBizarra:: (Number, Number)->Number
cuentaBizarra (numero1, numero2)
 | numero1 > numero2 = numero1 + numero2
 | (numero2 - numero1) > 10 = numero2 - numero1
 | (numero2 - numero1) < 10 = numero1 * numero2

esNotaBochazo:: Number->Bool
esNotaBochazo nota = nota < 6
aprobo:: (Number, Number)->Bool
aprobo (nota1, nota2) = esNotaBochazo nota1 && esNotaBochazo nota2
promociono:: (Number, Number)->Bool
promociono (nota1, nota2) = (nota1 + nota2) >= 15 && (nota1 >= 7 && nota2 >= 7)

notasFinales:: ((Number, Number), (Number, Number))->(Number, Number)
notasFinales ((nota1, nota2) , (recu1, recu2))
 | recu1 == -1 && recu2 == -1 = (nota1, nota2)
 | recu1 == -1 && recu2 >   0 = (nota1, recu2)
 | recu1 > 0   && recu2 == -1 = (recu1, nota2)
 | recu1 > 0   && recu2 > 0   = (recu1, recu2)

esMayorDe18:: (String, Number)->Bool
esMayorDe18 (nombre, edad) = (>21) edad

ultimaFuncion :: (Number, Number)->(Number, Number)
ultimaFuncion (numero1, numero2)
 | even numero1 && odd numero2 = ((*2) numero1, numero2 + 1)
 | even numero1                = ((*2) numero1, numero2)
 | odd numero2                 = (numero1, numero2 + 1)
