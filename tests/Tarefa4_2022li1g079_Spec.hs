module Tarefa4_2022li1g079_Spec where

import LI12223
import Tarefa4_2022li1g079
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1- É atropelado por isso devolve True" ~: True ~=? jogoTerminou (Jogo (Jogador (1,2)) (Mapa 4 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco]), (Estrada 2, [Carro, Carro, Carro, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum])]))
                                             ,"Teste 2- O jogador esta fora dos limites do mapa no x, por isso o jogo termina e da True" ~: True ~=? jogoTerminou (Jogo (Jogador (4,2)) (Mapa 4 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco]), (Estrada 2, [Carro, Carro, Carro, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum])]))
                                             ,"Teste 3- O jogador esta fora dos limites do mapa no y, por isso o jogo termina e da True" ~: True ~=? jogoTerminou (Jogo (Jogador (0,7)) (Mapa 4 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco]), (Estrada 2, [Carro, Carro, Carro, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum])]))
                                             ,"Teste 4- O jogador caiu na agua, por isso o jogo termina e da True" ~: True ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 4 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco]), (Estrada 2, [Carro, Carro, Carro, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum])]))
                                             ,"Teste 5- O jogador econtra-se num Tronco por isso nao morre, e da False" ~: False ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 4 [(Rio 2, [Nenhum, Tronco, Nenhum, Tronco]), (Estrada 2, [Carro, Carro, Carro, Nenhum]), (Relva, [Arvore, Nenhum, Arvore, Nenhum])]))]
