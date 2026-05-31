
data Participante = UnParticipante{
    nombreParticipante :: String,
    trucosDeCocina :: [Plato -> Plato],
    platoEspecialidad :: Plato
}

data Plato = UnPlato{
    nombrePlato :: String,
    dificultad :: Int,
    componentes :: [(String, Int)]
} deriving (Show, Eq)

pepeRoncino:: Participante
pepeRoncino = UnParticipante{
    nombreParticipante = "pepe_Roncino",
    trucosDeCocina = [darSabor 5 2, simplificar, duplicarPorcion],
    platoEspecialidad = pescadoCurado
}

pescadoCurado :: Plato
pescadoCurado = UnPlato{
    nombrePlato = "pescadoCurado",
    dificultad = 8,
    componentes = [("sal", 50),("pescado", 400),("salsa",50),("pimientaNegra",5),("pimientaBlanca",6)]
}

platinum :: Plato
platinum = UnPlato{
    nombrePlato = "platinum",
    dificultad = 10,
    componentes = map (\n -> ("ingrediente" ++ show n, n)) [1..]
    }
    
 {-risotto es un plato que use para probar cosas como esMejorQue-}
risotto :: Plato
risotto = UnPlato{
    nombrePlato = "risotto",
    dificultad = 7,
    componentes = [("arroz", 100), ("azafran", 6), ("azafran", 6), ("azafran", 6),("azafran", 6), ("azafran", 6)]

}

agregarComponentes :: (String, Int) -> [(String, Int)] -> [(String, Int)]
agregarComponentes nuevoComponente componentes = nuevoComponente : componentes 

cambiar :: (String, Int) -> Plato -> Plato
cambiar tupla unPlato = unPlato{componentes = agregarComponentes tupla (componentes unPlato)}  

endulzar :: Int -> Plato -> Plato
endulzar gramos = cambiar ("azucar", gramos)

salar :: Int -> Plato -> Plato
salar gramos = cambiar ("sal", gramos)

darSabor :: Int -> Int -> Plato -> Plato
darSabor azucar sal = endulzar azucar . salar sal

tuplaPorDos :: (a, Int) -> (a,Int)
tuplaPorDos (a, n) = (a, n *2)

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato{componentes = map tuplaPorDos (componentes unPlato)}

calcularComponentes :: Plato -> Int
calcularComponentes unPlato = length (componentes unPlato)

limiteComponentes :: Plato -> Int -> Bool
limiteComponentes unPlato limite = calcularComponentes unPlato >= limite

limiteDificultad :: Plato -> Int -> Bool
limiteDificultad unPlato limite = dificultad unPlato >= limite

criterios :: Plato -> Bool
criterios unPlato = limiteComponentes unPlato 5 && limiteDificultad unPlato 7

ingredienteIndispensable :: (a, Int) -> Bool
ingredienteIndispensable (a, n) = n>=10  

simplificar :: Plato -> Plato
simplificar unPlato
    |criterios unPlato = unPlato{dificultad = 5, componentes = filter ingredienteIndispensable (componentes unPlato) }
    |otherwise = unPlato


esVegano :: Plato -> Bool
esVegano unPlato = not $ tieneTalCosa unPlato "carne" || tieneTalCosa unPlato "huevo" 

esSinTacc :: Plato -> Bool
esSinTacc unPlato = not $ tieneTalCosa unPlato "harina"

tieneTalCosa :: Plato -> String -> Bool
tieneTalCosa unPlato talCosa = any ((==) talCosa . fst) (componentes unPlato) 

cantidadDe :: String -> Plato -> Int
cantidadDe ingrediente unPlato = sum . map snd . filter ((== ingrediente) . fst) $ componentes unPlato

esComplejo :: Plato -> Bool 
esComplejo = criterios

noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = cantidadDe "sal" unPlato > 2

cocinar :: Participante -> Plato
cocinar unParticipante = foldl(flip($))  (platoEspecialidad unParticipante)  (trucosDeCocina unParticipante)

esMasDificil :: Plato -> Plato -> Bool
esMasDificil unPlato otroPlato = dificultad unPlato > dificultad otroPlato

esMasPesado :: Plato -> Plato -> Bool
esMasPesado unPlato otroPlato = sum (map snd (componentes unPlato)) > sum (map snd (componentes otroPlato)) 

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && esMasPesado unPlato otroPlato

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante participante1 participante2
    | esMejorQue (cocinar participante1) (cocinar participante2) = participante1
    | otherwise = participante2

participanteEstrella :: [Participante] -> Participante
participanteEstrella = foldl1 mejorParticipante
