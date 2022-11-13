module Tarefa1_2022li1g079_Spec where

import LI12223
import Tarefa1_2022li1g079
import Test.HUnit


testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1- Três terrenos diferentes com os obstáculos corretos, caso normal" ~: True ~=? mapaValido (Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Estrada 2 , [Carro, Nenhum, Nenhum, Nenhum]), (Rio 2, [Tronco, Tronco, Nenhum, Nenhum])])
                                             ,"Teste 2- Tem um carro no terreno rio por isso é inválido" ~: False ~=? mapaValido (Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Estrada 2 , [Carro, Nenhum, Nenhum, Nenhum]), (Rio 2, [Tronco, Tronco, Nenhum, Carro])])
                                             ,"Teste 3- Tem um tronco no terreno Estrada assim, é inválido" ~: False ~=? mapaValido (Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Estrada 2 , [Carro, Nenhum, Nenhum, Tronco]), (Rio 2, [Tronco, Tronco, Nenhum, Nenhum])])
                                             ,"Teste 4- A largura do mapa nao coincide com o numero de obstaculos" ~: False ~=? mapaValido (Mapa 2 [(Relva, [Arvore, Nenhum, Arvore, Tronco]), (Estrada 2 , [Carro, Nenhum, Nenhum, Nenhum]), (Rio 2, [Tronco, Tronco, Nenhum, Carro])])
                                             ,"Teste 5- Existem dois rios contiguos com a mesma direçao" ~: False ~=? mapaValido (Mapa 4 [(Rio 2, [Tronco, Nenhum, Tronco, Carro]), (Rio 2, [Tronco, Tronco, Nenhum, Carro]), (Estrada 2 , [Carro, Nenhum, Nenhum, Nenhum])])
                                             ,"Teste 6- Existem  carros a ocupar 4 unidades seguidas" ~: False ~=? mapaValido (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Nenhum]), (Estrada 2 , [Carro, Carro, Carro, Carro, Nenhum]), (Rio 2, [Tronco, Tronco, Nenhum, Nenhum, Tronco])])
                                             ,"Teste 7- Existem troncos com mais de 6 unidades seguidas" ~: False ~=? mapaValido (Mapa 7 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum]), (Estrada 2 , [Carro, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Carro]), (Rio 2, [Tronco, Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum])])
                                             ,"Teste 8- Numa das linhas do mapa, nao existe nenhum obstaculo do tipo Nenhum" ~: False ~=? mapaValido (Mapa 4 [(Rio 2, [Tronco, Tronco, Tronco, Tronco]), (Relva, [Arvore, Arvore, Nenhum, Nenhum]), (Estrada 2 , [Carro, Nenhum, Nenhum, Nenhum])])
                                             ,"Teste 9- Existem mais de cinco relvas seguidas" ~: False ~=? mapaValido (Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum]), (Estrada 2 , [Carro, Nenhum, Nenhum, Nenhum]), (Rio 2, [Tronco, Tronco, Nenhum, Carro])])]