{- |
Module      : Tarefa3_2022li1g079
Description : Movimentação do personagem e obstáculos
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g079 where

import LI12223

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo = undefined






{- | a função posT determina as coordenadas de todos os troncos num dado mapa, para o fazer utiliza a função auxiliar posTaux
-}


posT :: Mapa -> Coordenadas -> [Coordenadas]
posT (Mapa l []) (x,y) = []
posT (Mapa l ((Rio v,ob):t)) (x,y) = posTaux (Rio v, ob) (x,y) ++ posT (Mapa l t) ((x+1),y)
posT (Mapa l ((Relva,ob):t)) (x,y) = posT (Mapa l t) ((x+1),y)
posT (Mapa l ((Estrada v,ob):t)) (x,y) = posT (Mapa l t) ((x+1),y)

{- | a função posTaux é uma função auxiliar que determina as coordenadas de todos os troncos numa linha do mapa
-}

posTaux :: (Terreno,[Obstaculo]) -> Coordenadas -> [Coordenadas]
posTaux (Rio v,[]) (x,y) = []
posTaux (Rio v, (h:t)) (x,y) | h == Tronco = ((x,y):posTaux (Rio v, t) (x,(y+1)))
                             | otherwise = posTaux (Rio v, t) (x,(y+1))



{- | a função posA determina as coordenadas de todas as árvores num dado mapa, para o fazer utiliza a função auxiliar posAaux
-}


posA :: Mapa -> Coordenadas -> [Coordenadas]
posA (Mapa l []) (x,y) = []
posA (Mapa l ((Relva,ob):t)) (x,y) = posAaux (Relva,ob) (x,y) ++ posA (Mapa l t) ((x+1),y)
posA (Mapa l ((Rio v,ob):t)) (x,y) = posA (Mapa l t) ((x+1),y)
posA (Mapa l ((Estrada v,ob):t)) (x,y) = posA (Mapa l t) ((x+1),y) 

{- | a função posAaux é uma função auxiliar que determina as coordenadas de árvores numa linha do mapa
-}

posAaux :: (Terreno,[Obstaculo]) -> Coordenadas -> [Coordenadas]
posAaux (Relva,[]) (x,y) = []
posAaux (Relva,(h:t)) (x,y) | h == Arvore = ((x,y):posAaux (Relva,t) (x,(y+1)))
                            | otherwise = posAaux (Relva,t) (x,(y+1))





