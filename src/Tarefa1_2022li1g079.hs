{- |
Module      : Tarefa1_2022li1g079
Description : Validação de um mapa
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g079 where

import LI12223

mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa c []) = True
mapaValido1 (Mapa c (((Estrada n),(x:y)):t)) 
    |(elem Arvore (x:y) || elem Tronco (x:y)) = False
    |otherwise = mapaValido1 (Mapa c t)
mapaValido1 (Mapa c (((Rio n),(x:y)):t)) 
    |(elem Carro (x:y) || elem Arvore (x:y)) = False
    |otherwise = mapaValido1 (Mapa c t)     
mapaValido1 (Mapa c (( Relva, (x:y)):t)) 
    |(elem Tronco (x:y) || elem Carro (x:y)) = False
    | otherwise = mapaValido1 (Mapa c t)

mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa c []) = True
mapaValido2 (Mapa c ((Rio n, (x:y)):[])) = True
mapaValido2 (Mapa c ((Relva, (x:y)):t)) = mapaValido2 (Mapa c t)
mapaValido2 (Mapa c ((Estrada n, (x:y)):t)) = mapaValido2 (Mapa c t)
mapaValido2 (Mapa c ((Rio n, (x:y)): (Relva,  (x':y')):t)) = mapaValido2 (Mapa c t)
mapaValido2 (Mapa c ((Rio n, (x:y)): (Estrada n', (x':y')):t)) = mapaValido2 (Mapa c t) 
mapaValido2 (Mapa c ((Rio n, (x:y)): (Rio n', (x':y')): t))
    | n > 0  && n' >0 = False
    | n < 0 && n' < 0 = False
    |otherwise = mapaValido2 (Mapa c ((Rio n', (x':y')):t))

mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa c []) = True
mapaValido3 (Mapa c ((Relva, (x:y)):t)) = mapaValido3 (Mapa c t)
mapaValido3 (Mapa c ((Estrada n, (x:y)):t)) = mapaValido3 (Mapa c t) 
mapaValido3 (Mapa c ((Rio n, (x:y)):t)) | contaAux (conta (x:y)) > 5 = False
                                        | otherwise = mapaValido3 (Mapa c t)


conta :: [Obstaculo] -> [[Obstaculo]] 
conta [] = [] 
conta [x] = [[x]]
conta (x:y:t) | elem x (head r ) = (x:(head r)) : tail r
              | otherwise = [x] :r
                where r = conta (y:t)

contaAux :: [[Obstaculo]] -> Int
contaAux [x] = length x
contaAux [] = 0
contaAux ((x:y):t) | (elem Tronco (x:y)) && (length (x:y) >= (contaAux t)) = length (x:y)
                   | otherwise = contaAux t


mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa c []) = True
mapaValido4 (Mapa c ((Relva, (x:y)):t)) = mapaValido4 (Mapa c t)
mapaValido4 (Mapa c ((Rio n, (x:y)):t)) = mapaValido4 (Mapa c t) 
mapaValido4 (Mapa c ((Estrada n, (x:y)):t)) | contaAux1 (conta (x:y)) > 3 = False
                                            | otherwise = mapaValido4 (Mapa c t)


contaAux1 :: [[Obstaculo]] -> Int
contaAux1 [x] = length x
contaAux1 [] = 0
contaAux1 ((x:y):t) | (elem Carro (x:y)) && (length (x:y) >= (contaAux1 t)) = length (x:y)
                    | otherwise = contaAux1 t                                        
                    