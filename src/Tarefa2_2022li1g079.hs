{- |
Module      : Tarefa2_2022li1g079
Description : Geração contínua de um mapa
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g079 where

import LI12223

{- | A função estendeMapa é responsável por gerar novas linhas válidas para o mapa, assim como obstaculos para as novas linhas. Para o fazer
utiliza as funções auxiliares geradorT e geradorObs
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ls) num = Mapa l ((geradorT (Mapa l ls) num , geradorObs (geradorT (Mapa l ls) num) [] l num): ls)

{- |A função geradorT é responsável por gerar novos terrenos com velocidades aleatórias utiliza para isso a função geradorT e geradorObs-}

geradorT :: Mapa -> Int -> Terreno
geradorT (Mapa l ls) num | prox == Rio 0 = (Rio ((mod num (div l 2)) + 3))
                         | prox == Estrada 0 = (Estrada ((mod num (div l 2)) + 1))
                         | prox == Relva = Relva
    where prox = proximosTerrenosValidos (Mapa l ls) !! mod (num + length ls) (length (proximosTerrenosValidos (Mapa l ls)))

{- | A função geradorObs é responsável por gerar uma lista aleatoria de novos obstaculos, tendo em conta as restrições da
função proximosObstaculosValidos 
-}

geradorObs :: Terreno -> [Obstaculo] -> Int -> Int -> [Obstaculo]
geradorObs (Rio vel) obs l num | obs == [] = geradorObs (Rio vel) ([[Nenhum,Tronco] !! (mod num 2)]) l num
                               | length obs == l = obs
                               | otherwise = geradorObs (Rio vel) prox l num
    where prox = (proximosObstaculosValidos l (Rio vel, obs)) !! mod (num + length obs) (length (proximosObstaculosValidos l (Rio vel, obs))) : obs

geradorObs (Estrada vel) obs l num | obs == [] = geradorObs (Estrada vel) ([[Nenhum,Carro] !! (mod num 2)]) l num
                                   | length obs == l = obs
                                   | otherwise = geradorObs (Estrada vel) prox l num
    where prox = (proximosObstaculosValidos l (Estrada vel, obs)) !! mod (num + length obs) (length (proximosObstaculosValidos l (Estrada vel, obs))) : obs

geradorObs (Relva) obs l num | obs == [] = geradorObs Relva ([[Nenhum,Arvore] !! (mod num 2)]) l num
                             | length obs == l = obs 
                             | otherwise = geradorObs Relva prox l num
    where prox = (proximosObstaculosValidos l (Relva, obs)) !! mod (num + length obs) (length (proximosObstaculosValidos l (Relva, obs))) : obs




{- | A função proximosTerrenosValido recebe um mapa e dá uma lista de todos os terrenos seguintes possiveis -}


proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l []) = [Estrada 0, Rio 0, Relva]
proximosTerrenosValidos (Mapa l ((Rio y,x):(Rio y1,x1):(Rio y2,x2):(Rio y3,x3):t)) = [Estrada 0,Relva]
proximosTerrenosValidos (Mapa l ((Rio y,x):t)) = [Estrada 0, Rio 0, Relva]
proximosTerrenosValidos (Mapa l ((Estrada y,x):(Estrada y1,x1):(Estrada y2,x2):(Estrada y3,x3):(Estrada y4,x4):t)) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa l ((Relva , x0):(Relva , x1):(Relva , x2):(Relva , x3):(Relva , x4):t)) = [Estrada 0, Rio 0]
proximosTerrenosValidos (Mapa l ((Estrada y,x):t)) = [Estrada 0, Rio 0, Relva ]
proximosTerrenosValidos (Mapa l ((Relva, x):t)) = [Estrada 0, Rio 0, Relva]


{- | A função proximosObstaculosValidos é responsável por gerar a lista de possoveis seguintes obstaculos, respeitando as restrições.
Para isso utiliza uma função auxiliar proximosObstaculosValidosaux
-}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (x,y) | length y == n = []
                                  | length y == (n-1) && (elem Nenhum y) == False = [Nenhum]
                                  | otherwise = proximosObstaculosValidosaux n (x,y)


{- | A função proximosObstaculosValidosaux é a função auxiliar da função proximosObstaculosValidos, e é responsável por 
restringir a nova lista de obstaculos tendo em conta as restrições dadas
-}

proximosObstaculosValidosaux :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidosaux n ((Rio y),(Tronco:Tronco:Tronco:Tronco:Tronco:t)) = [Nenhum]
proximosObstaculosValidosaux n ((Rio y),x) = [Nenhum, Tronco]
proximosObstaculosValidosaux n ((Relva),x) = [Nenhum, Arvore]
proximosObstaculosValidosaux n ((Estrada y),(Carro:Carro:Carro:t)) = [Nenhum]
proximosObstaculosValidosaux n ((Estrada y),x) = [Nenhum, Carro]











