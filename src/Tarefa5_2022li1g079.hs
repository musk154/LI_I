{- |
Module      : Tarefa5_2022li1g079
Description : Geraçao continua de um mapa ao longo do jogo
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g079 where

import Tarefa2_2022li1g079
import LI12223


{- | A funçao deslizaJogo faz o mapa deslizar ao longo do tempo onde a última linha do mapa é retirada e adicionada uma nova linha,
gerada aleatóriamente, no topo do mapa. Utiliza a funçao estende mapa para tal.  -}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo num (Jogo (Jogador (x,y)) (Mapa l ls)) = (Jogo (Jogador (x,y+1)) (estendeMapa (Mapa l (init ls)) num)) 

