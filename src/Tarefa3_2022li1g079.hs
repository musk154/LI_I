{- |
Module      : Tarefa3_2022li1g079
Description : Movimentação do personagem e obstáculos
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g079 where

import LI12223

{- | A função animaJogo é responsável por movimentar o jogador (segundo a jogada) e os obstáculos (segundo as velocidades)
-}

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo pl mapa) jog = (Jogo (player pl mapa jog) (animaObs mapa))




{- | a função player determina as próximas coordenadas do jogador, consoante a sua posição no mapa e a existência de obstaculos,
na primeira linha é verificado que se o jogador se encontrar em cima de um tronco e não efetuar nenhuma jogada (Parado), 
movimenta-se consoante a velocidade do tronco.

Obs: na ultima linha o "otherwise" representa o movimento "Esquerda"
-}




player :: Jogador -> Mapa -> Jogada -> Jogador
player (Jogador (x,y)) (Mapa l tl) (Parado) | elem (x,y) (posT (Mapa l tl) ((-900),(515))) = (Jogador (posMoveT (x,y) (Mapa l tl)))
                                            | otherwise = (Jogador (x,y))

player (Jogador (x,y)) (Mapa l tl) (Move jog) | jog == Cima && elem (x,y+90) (posA (Mapa l tl) ((-900),(515))) = (Jogador (x,y))
                                              | jog == Baixo && elem (x,y-90) (posA (Mapa l tl) ((-900),(515))) = (Jogador (x,y))
                                              | jog == Direita && elem (x+90,y) (posA (Mapa l tl) ((-900),(515))) = (Jogador (x,y))
                                              | jog == Esquerda && elem ((x-90),y) (posA (Mapa l tl) ((-900),(515))) = (Jogador (x,y))
                                              | jog == Cima = (Jogador (x,y+90))
                                              | jog == Baixo = (Jogador (x,y-90))
                                              | jog == Direita = (Jogador (x+90,y))
                                              | otherwise = (Jogador (x-90,y))




{- | a função posT determina as coordenadas de todos os troncos num dado mapa, para o fazer utiliza a função auxiliar posTaux
-}


posT :: Mapa -> Coordenadas -> [Coordenadas]
posT (Mapa l []) (x,y) = []
posT (Mapa l ((Rio v,ob):t)) (x,y) = posTaux (Rio v, ob) (x,y) ++ posT (Mapa l t) (x,(y-90))
posT (Mapa l ((Relva,ob):t)) (x,y) = posT (Mapa l t) (x,(y-90))
posT (Mapa l ((Estrada v,ob):t)) (x,y) = posT (Mapa l t) (x,(y-90))

{- | a função posTaux é uma função auxiliar que determina as coordenadas de todos os troncos numa linha do mapa
-}

posTaux :: (Terreno,[Obstaculo]) -> Coordenadas -> [Coordenadas]
posTaux (Rio v,[]) (x,y) = []
posTaux (Rio v, (h:t)) (x,y) | h == Tronco = ((x,y):posTaux (Rio v, t) ((x+90),y))
                             | otherwise = posTaux (Rio v, t) ((x+90),y)



{- | a função posA determina as coordenadas de todas as árvores num dado mapa, para o fazer utiliza a função auxiliar posAaux
-}

posA :: Mapa -> Coordenadas -> [Coordenadas]
posA (Mapa l []) (x,y) = []
posA (Mapa l ((Relva,ob):t)) (x,y) = posAaux (Relva,ob) (x,y) ++ posA (Mapa l t) (x,(y-90))
posA (Mapa l ((Rio v,ob):t)) (x,y) = posA (Mapa l t) (x,(y-90))
posA (Mapa l ((Estrada v,ob):t)) (x,y) = posA (Mapa l t) (x,(y-90)) 

{- | a função posAaux é uma função auxiliar que determina as coordenadas de árvores numa linha do mapa
-}

posAaux :: (Terreno,[Obstaculo]) -> Coordenadas -> [Coordenadas]
posAaux (Relva,[]) (x,y) = []
posAaux (Relva,(h:t)) (x,y) | h == Arvore = ((x,y):posAaux (Relva,t) ((x+90),y))
                            | otherwise = posAaux (Relva,t) ((x+90),y)

{- | a função posMoveT é a função responsável por obter as proximas coordenadas de um tronco, para mais tarde ser usado no caso
do jogador estar em cima de um tronco, se movimentar com ele -}

--posOndeFoiTronco
posMoveT :: Coordenadas -> Mapa -> Coordenadas
posMoveT (x,y) (Mapa l ls) = ((x+90),y) 



{- | A função posMoveVelocidadeAux é a função auxiliar da função posMoveVelocidade, responsável por obter as coordenadas
do tronco consoante a velocidade
Obs: Só utiliza um Int pois os troncos movimentam-se na horizontal, logo só vai mudar o y
-}




{- | a função animaObs é responsável por movimentar todos os obstáculos presentes num dado mapa, utiliza para isso uma
função auxiliar animaObsAux -}


animaObs :: Mapa -> Mapa
animaObs (Mapa l ls) = (Mapa l (animaObsAux ls))

{- | a função animaObsAux é uma função auxiliar da função animaObs e é responsável por movimentar os obstaculos de uma lista de (Terreno,[Obstaculo])
tendo em conta a velocidade do Terreno, e fazendo os obstaculos que desaparecem de um lado do mapa aparecerem do lado oposto -}

animaObsAux :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
animaObsAux [] = []
animaObsAux ((Relva, obs):t) = (Relva, obs):animaObsAux t
animaObsAux ((Rio vel, obs):t) | vel <= 0 = (Rio vel, ((drop (-vel) obs) ++ (take (-vel) obs))):animaObsAux t
                               | otherwise = (Rio vel, ((drop ((length obs) - vel) obs) ++ (take ((length obs) - vel) obs))): animaObsAux t
animaObsAux ((Estrada vel, obs):t) | vel <= 0 = (Estrada vel, ((drop (-vel) obs) ++ (take (-vel) obs))): animaObsAux t
                                   | otherwise = (Estrada vel, ((drop ((length obs) - vel) obs) ++ (take ((length obs) - vel) obs))): animaObsAux t



{- | a função posMoveVelocidade é uma função auxiliar da função posMoveT que verifica se a proxima posição de um tronco
respeita as fronteiras do mapa -}













