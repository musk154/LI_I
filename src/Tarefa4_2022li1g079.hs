{- |
Module      : Tarefa4_2022li1g079
Description : Determinar se o jogo terminou
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g079 where

import LI12223

{-| A funçao jogoTerminou verifica se o jogador terminou o jogo ou nao de acordo com: as coordenadas nao coicidirem com os limites do mapa, estar em nenhum no terreno rio e ter as mesmas coordenadas que um carro no terreno estrada.
Assim utiliza tres funçoes auxiliares definidas a baixo para determinar a sua validade.
@jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa c ((tr, (x':y')):t))) = jogoTerminoumapa (x,y) (Mapa c ((tr, (x':y')):t)) || jogoTerminouAgua (x,y) (Mapa c ((tr, (x':y')):t)) || jogoTerminouCarro (x,y) (Mapa c ((tr, (x':y')):t))
@-}

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa c ((tr, (x':y')):t))) = jogoTerminoumapa (x,y) (Mapa c ((tr, (x':y')):t)) || jogoTerminouAgua (x,y) (Mapa c ((tr, (x':y')):t)) || jogoTerminouCarro (x,y) (Mapa c ((tr, (x':y')):t))

{-|
A funçao jogoTerminoumapa verifica se o jogador/ personagem se encontra fora dos limites do mapa. Assim, recebe as coordenadas do jogador, o mapa para comparaçao e devolve um Bool. Caso seja True, o jogador perdeu o jogo.
Comparamos: o x (posiçao do jogador na linha- verticalmente) com a largura do mapa, ou seja, quantos obstaculos estao definidos; o y (posiçao do jogador na coluna- horizontalmente) com o comprimento da lista de [Terreno, (Obstaculos)] (quantos terrenos existem). 
Como a primeira posiçao é 0 e nao 1, usamos o >= (maior ou igual) pois no caso de serem iguais o jogador estara fora do mapa. 
Basta uma das coordenadas estar fora do mapa para o jogador perder entao usamos || que é ou. Se alguma das condiçoes for verdadeira a funçao será verdadeira o que significa que o jogador perdeu, caso contrário é falso, continua o jogo.
@jogoTerminoumapa :: Coordenadas -> Mapa -> Bool
jogoTerminoumapa (x,y) (Mapa c ((tr, (x':y')):t)) | x >= c || y >= length ((tr, (x':y')):t) = True
                                                  | otherwise = False
@-}

jogoTerminoumapa :: Coordenadas -> Mapa -> Bool
jogoTerminoumapa (x,y) (Mapa c ((tr, (x':y')):t)) | x >= c || y >= length ((tr, (x':y')):t) = True
                                                  | otherwise = False

{-|
A funçao jogoTerminouAgua determina se o jogo termina pois o jogador caiu na agua ou nao.
Se o primeiro terreno for rio, utilizando uma funçao auxiliar (ondeTronco) que indica as coordenadas todas de onde nao ha tronco no mapa, ou seja, onde tem Nenhum, a funçao irá procurar se nas coordenadas dadas (do jogador) existe nessas coordenadas um tronco.
Caso exista um tronco nessas coordenadas, o jogador nao morre, ou seja false. De seguida verifica o resto da funçao mas com as coordenadas (x-1,y) pois a linha analid¡sada com o movimento do mapa desaparece e passamos para a seguinte.
Quando a funçao encontra os terrenos estrada ou relva, procura no resto do mapa pelo rio, caso haja.
@jogoTerminouAgua :: Coordenadas -> Mapa -> Bool
jogoTerminouAgua (x,y) (Mapa c []) = False
jogoTerminouAgua (x,y) (Mapa c ((Rio n, (x' :y')):t)) = if elem (x,y) (ondenaoTronco (Rio n, (x': y')) (0,0)) then True else jogoTerminouAgua (x-1,y) (Mapa c t)  
jogoTerminouAgua (x,y) (Mapa c ((Estrada n, (x' :y')):t)) = jogoTerminouAgua (x-1,y) (Mapa c t)
jogoTerminouAgua (x,y) (Mapa c ((Relva , (x' :y')):t)) = jogoTerminouAgua (x-1,y) (Mapa c t)

ondenaoTronco :: (Terreno, [Obstaculo]) -> Coordenadas -> [Coordenadas]
ondenaoTronco (Rio n, []) (x,y) = []
ondenaoTronco (Rio n, (x':y')) (x,y) = if x' == Nenhum then (x,y) : ondenaoTronco (Rio n, y') (x, y+1) else ondenaoTronco (Rio n, y') (x, y+1)
@-}

jogoTerminouAgua :: Coordenadas -> Mapa -> Bool
jogoTerminouAgua (x,y) (Mapa c []) = False
jogoTerminouAgua (x,y) (Mapa c ((Rio n, (x' :y')):t)) = if elem (x,y) (ondenaoTronco (Rio n, (x': y')) (0,0)) then True else jogoTerminouAgua (x-1,y) (Mapa c t)  
jogoTerminouAgua (x,y) (Mapa c ((Estrada n, (x' :y')):t)) = jogoTerminouAgua (x-1,y) (Mapa c t)
jogoTerminouAgua (x,y) (Mapa c ((Relva , (x' :y')):t)) = jogoTerminouAgua (x-1,y) (Mapa c t)

ondenaoTronco :: (Terreno, [Obstaculo]) -> Coordenadas -> [Coordenadas]
ondenaoTronco (Rio n, []) (x,y) = []
ondenaoTronco (Rio n, (x':y')) (x,y) = if x' == Nenhum then (x,y) : ondenaoTronco (Rio n, y') (x, y+1) else ondenaoTronco (Rio n, y') (x, y+1)

{-|
A semelhança da funçao anterior, a funçao jogoTerminouCarro verifica se o jogador se encontra nas mesmas coordenadas que um carro, ou seja é atropelado ou nao.
Tambem utiliza uma funçao auxiliar que dá as coordenadas dos carros (ondeCarro).
Neste caso, compara as coordenadas do jogador com as coordenadas obtidas na funçao auxiliar mas no caso de coicidirem, o jogo termina pois o jogador é atropelado.
No caso do terreno ser rio ou relva a funçao analisa o resto do mapa, comparando com as coordenada do jogador, onde o terreno for estrada.
@jogoTerminouCarro :: Coordenadas -> Mapa -> Bool
jogoTerminouCarro (x,y) (Mapa c []) = False
jogoTerminouCarro (x,y) (Mapa c ((Estrada n, (x' :y')):t)) = if elem (x,y) (ondeCarro (Estrada n, (x': y')) (0,0)) then True else jogoTerminouCarro (x-1,y) (Mapa c t)  
jogoTerminouCarro (x,y) (Mapa c ((Rio n, (x' :y')):t)) = jogoTerminouCarro (x-1,y) (Mapa c t)
jogoTerminouCarro (x,y) (Mapa c ((Relva , (x' :y')):t)) = jogoTerminouCarro (x-1,y) (Mapa c t)

ondeCarro :: (Terreno, [Obstaculo]) -> Coordenadas -> [Coordenadas]
ondeCarro (Estrada n, []) (x,y) = []
ondeCarro (Estrada n, (x':y')) (x,y) = if x' == Carro then (x,y) : ondeCarro (Estrada n, y') (x, y+1) else ondeCarro (Estrada n, y') (x, y+1)@-}

jogoTerminouCarro :: Coordenadas -> Mapa -> Bool
jogoTerminouCarro (x,y) (Mapa c []) = False
jogoTerminouCarro (x,y) (Mapa c ((Estrada n, (x' :y')):t)) = if elem (x,y) (ondeCarro (Estrada n, (x': y')) (0,0)) then True else jogoTerminouCarro (x-1,y) (Mapa c t)  
jogoTerminouCarro (x,y) (Mapa c ((Rio n, (x' :y')):t)) = jogoTerminouCarro (x-1,y) (Mapa c t)
jogoTerminouCarro (x,y) (Mapa c ((Relva , (x' :y')):t)) = jogoTerminouCarro (x-1,y) (Mapa c t)

ondeCarro :: (Terreno, [Obstaculo]) -> Coordenadas -> [Coordenadas]
ondeCarro (Estrada n, []) (x,y) = []
ondeCarro (Estrada n, (x':y')) (x,y) = if x' == Carro then (x,y) : ondeCarro (Estrada n, y') (x, y+1) else ondeCarro (Estrada n, y') (x, y+1)