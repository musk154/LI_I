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

{- | a função player determina as próximas coordenadas do jogador, consoante a sua posição no mapa e a existência de obstaculos,
na primeira linha é verificado que se o jogador se encontrar em cima de um tronco e não efetuar nenhuma jogada (Parado), 
movimenta-se consoante a velocidade do tronco.

Obs: na ultima linha o "otherwise" representa o movimento "Esquerda"
-}


player :: Jogador -> Mapa -> Jogada -> Jogador
player (Jogador (x,y)) (Mapa l tl) (Parado) | elem (x,y) (posT (Mapa l tl) (0,0)) = (Jogador (posMoveT (x,y) (Mapa l tl)))
                                            | otherwise = (Jogador (x,y))

player (Jogador (x,y)) (Mapa l tl) (Move jog) | jog == Cima && elem ((x-1),y) (posA (Mapa l tl) (0,0)) = (Jogador (x,y))
                                              | jog == Baixo && elem ((x+1),y) (posA (Mapa l tl) (0,0)) = (Jogador (x,y))
                                              | jog == Direita && elem (x,(y+1)) (posA (Mapa l tl) (0,0)) = (Jogador (x,y))
                                              | jog == Esquerda && elem (x,(y-1)) (posA (Mapa l tl) (0,0)) = (Jogador (x,y))
                                              | jog == Cima && x == 0 = (Jogador (x,y))
                                              | jog == Baixo && x == ((length tl) - 1) = (Jogador (x,y))
                                              | jog == Direita && y == (l-1) = (Jogador (x,y))
                                              | jog == Esquerda && y == 0 = (Jogador (x,y))
                                              | jog == Cima = (Jogador ((x-1),y))
                                              | jog == Baixo = (Jogador ((x+1),y))
                                              | jog == Direita = (Jogador (x,(y+1)))
                                              | otherwise = (Jogador (x,(y-1)))




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

{- | a função posMoveT é a função responsável por movimentar os troncos do mapa consoante a velocidade do terreno -}


posMoveT :: Coordenadas -> Mapa -> Coordenadas
posMoveT (x,y) (Mapa l []) = (x,y)
posMoveT (x,y) (Mapa l ((Relva,ob):t)) = posMoveT (x,y) (Mapa l t) 
posMoveT (x,y) (Mapa l ((Estrada vel,ob):t)) = posMoveT (x,y) (Mapa l t)
posMoveT (x,y) (Mapa l ((Rio vel, ob):t)) | elem (x,y) (posT (Mapa l ((Rio vel, ob):t)) (0,0)) = posMoveVelocidade (x,y) (Mapa l ((Rio vel, ob):t))
                                          | otherwise = posMoveT (x,y) (Mapa l t)


{- | a função posMoveVelocidade é uma função auxiliar da função posMoveT que verifica se a proxima posição de um tronco
respeita as fronteiras do mapa -}


posMoveVelocidade :: Coordenadas -> Mapa -> Coordenadas
posMoveVelocidade (x,y) (Mapa l ((Rio vel,ob):t)) | y + vel < 0 = (x,(y+(l-vel)))
                                                  | y + vel >= l = (x,((y-l)+vel))
                                                  | otherwise = (x,y+vel)




















