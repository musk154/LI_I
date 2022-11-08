{- |
Module      : Tarefa2_2022li1g079
Description : Geração contínua de um mapa
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g079 where

import LI12223

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa m n = 

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l []) = [Estrada 0, Rio 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio y,x):(Rio y1,x1):(Rio y2,x2):(Rio y3,x3):t)) = [Estrada 0,Relva]
proximosTerrenosValidos (Mapa l ((Rio y,x):t)) = [Estrada 0, Rio 0, Relva]
proximosTerrenosValidos (Mapa l ((Estrada y,x):(Estrada y1,x1):(Estrada y2,x2):(Estrada y3,x3):(Estrada y4,x4))) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa l ((Relva x):(Relva x1):(Relva x2):(Relva x3):(Relva x4))) = [Estrada 0, Rio 0]
proximosTerrenosValidos (Mapa l ((Estrada y,x):t)) = [Estrada 0, Rio 0, Relva ]
proximosTerrenosValidos (Mapa l ((Relva x)):t) = [Estrada 0, Rio 0, Relva]




proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (x,y) | length x == n = []
                                  | length x == (n-1) && (elem Nenhum x) == False = [Nenhum]
                                  | otherwise = proximosObstaculosValidos n (x,y)

proximosObstaculosValidosaux :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidosaux n ((Rio y),x) = [Nenhum, Tronco]
proximosObstaculosValidosaux n ((Relva),x) = [Nenhum, Arvore]
proximosObstaculosValidosaux n ((Rio y),(Tronco:Tronco:Tronco:Tronco:Tronco:t)) = [Nenhum]
proximosObstaculosValidosaux n ((Estrada y),(Carro:Carro:Carro:t)) = [Nenhum]
proximosObstaculosValidosaux n ((Estrada y),x) = [Nenhum, Carro]














