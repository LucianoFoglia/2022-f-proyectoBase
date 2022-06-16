module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Mago = Mago {
    nombre::String,
    horrocruxes:: [Horrocrux]
}deriving (Show,Eq)

data Horrocrux = Horrocrux {
    denominacion::String,
    mago::Mago
}deriving (Show,Eq)

diadema = Horrocrux {
    denominacion = "Ravenclow",
    mago = srTenebroso
}

diario = Horrocrux {
    denominacion = "Diario de Tom Riddle",
    mago = srTenebroso
}

harry = Horrocrux {
    denominacion = "Harry Postre",
    mago = srTenebroso
}

srTenebroso = Mago {
    nombre = "Voldemort",
    horrocruxes = [diadema, diario, harry]
}

{-
    Al momento de destruir un horrocrux, sin estar presente el mago al que corresponde dicho horrocrux de todas maneras sufre las consecuencias. 
    Para ello, se pide definir la siguiente funcion:
    destruir:: Horrocrux -> Mago
    donde el mago que se retorna es el que corespondía al horrocrux, pero ya sin dicho horrocrux en su poder. Se asume que la denominación de cada 
    horrocrux es única.
    Por ejemplo, al destruir el diario de Tom Riddle, el mago tenebroso permance con vida, con los horrocruxes de la diadema y el mismo harry. 
-}

destruir :: Horrocrux -> Mago
destruir horro = (mago horro){horrocruxes = sacarHorrorcrux ((horrocruxes . mago) horro) horro}



{-
    La función principal es:
    finalFeliz:: [Horrocrux] -> Bool
    Si aún habiendo destruido todos los horrocruxes recibidos el mago tenebroso permaneciera con vida, el final no es feliz. En caso que en algún momento, el horrocrux que se destruye sea el último y en consecuencia el mago pierda su vida, se considera un final feliz (incluso si quedaran otros horrocruxes en la lista).
    Ejemplos:
    finalFeliz  [diadema, diario]
    False

    finalFeliz  [diario, diadema, harry]
    True

    finalFeliz  [diario, diadema, harry, otroMas]
    True
-}

{-
finalFeliz:: [Horrocrux] -> Bool
finalFeliz (h:hs)
-}

sacarHorrorcrux :: [Horrocrux] -> Horrocrux -> [Horrocrux]
sacarHorrorcrux (h1:hs) horrocruxASacar
    | denominacion h1 == denominacion horrocruxASacar = hs
    | otherwise = h1 : sacarHorrorcrux hs horrocruxASacar 

sacarHorrorcruxes :: [Horrocrux] -> [Horrocrux] -> [Horrocrux]
sacarHorrorcruxes horros [] = horros
sacarHorrorcruxes horros (h1:hs) = sacarHorrorcruxes (sacarHorrorcrux horros h1) hs 

noTieneMasHorrorcruxes :: Mago -> Bool
noTieneMasHorrorcruxes mago = cantHorrorcruxes mago == 0

cantHorrorcruxes :: Mago -> Number
cantHorrorcruxes = length . horrocruxes


sacarNumero :: [Number] -> Number -> [Number]
sacarNumero (n1:ns) num
    | n1 == num = ns
    | otherwise = n1 : sacarNumero ns num

sacarNumeros :: [Number] -> [Number] -> [Number]
sacarNumeros lista [] = lista
sacarNumeros lista (n1:ns) = sacarNumeros (sacarNumero lista n1) ns