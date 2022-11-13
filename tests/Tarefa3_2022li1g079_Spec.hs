module Tarefa3_2022li1g079_Spec where

import LI12223
import Tarefa3_2022li1g079
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1 - Teste com velocidade 0 nos terrenos e o jogador consegue mover-se" ~: Jogo (Jogador (0,0)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Nenhum])])) (Move Cima),
                                              "Teste 2 - Teste com o jogador no limite superior do mapa" ~: Jogo (Jogador (0,0)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Nenhum])])) (Move Cima),
                                              "Teste 3 - Teste com o jogador 'bloqueado' por uma arvore" ~: Jogo (Jogador (2,2)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,2)) (Mapa 5 [(Rio 0,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Relva, [Nenhum,Arvore,Arvore,Nenhum,Nenhum])])) (Move Cima),
                                              "Teste 4 - A jogada é 'Parado' e o jogador está em cima de um tronco, num rio com velocidade não nula" ~: Jogo (Jogador (0,1)) (Mapa 5 [(Rio 1,[Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 5 [(Rio 1,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum])])) (Parado),
                                              "Teste 5 - O jogador parado numa estrada com velocidade" ~: Jogo (Jogador (0,2)) (Mapa 5 [(Rio 1,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 5 [(Rio 1,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro,Carro,Nenhum,Nenhum])])) (Parado),
                                              "Teste 6 - Teste com o jogador no limite inferior do mapa" ~: Jogo (Jogador (1,1)) (Mapa 5 [(Rio 1,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2,[Nenhum,Nenhum,Nenhum,Carro,Carro])]) ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 5 [(Rio 1,[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro,Carro,Nenhum,Nenhum])])) (Move Baixo) ]
                                              
