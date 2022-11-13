module Tarefa2_2022li1g079_Spec where

import LI12223
import Tarefa2_2022li1g079
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1 - Testa Proximos Terrenos com 4 Rios seguidos" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Rio 0,[Nenhum,Nenhum]),(Rio 0,[Tronco,Nenhum]),(Rio 0, [Nenhum,Tronco]),(Rio 0, [Nenhum,Tronco])]),
                                              "Teste 2 - Testa Proximos Terrenos com 5 Estradas seguidas" ~: [Rio 0, Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Estrada 0,[Nenhum,Carro]),(Estrada 0, [Carro,Nenhum]),(Estrada 0, [Nenhum,Carro]),(Estrada 0,[Nenhum,Nenhum]),(Estrada 0,[Carro,Nenhum])]),
                                              "Teste 3 - Testa Proximos Terrenos com 5 Relvas seguidas" ~: [Estrada 0, Rio 0] ~=? proximosTerrenosValidos (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva,[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum])]),
                                              "Teste 4 - Testa Proximos Obstaculos com 3 Carros seguidos" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Estrada 0, [Carro,Carro,Carro,Nenhum]),
                                              "Teste 5 - Testa Proximos Obstaculos com 5 Troncos seguidos" ~: [Nenhum] ~=? proximosObstaculosValidos 7 (Rio 0, [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum]),
                                              "Teste 6 - Testa Proximos Obstaculos se uma linha não tiver nenhum 'Nenhum' e só faltar preencher um espaço" ~: [Nenhum] ~=? proximosObstaculosValidos 5 (Relva, [Arvore,Arvore,Arvore,Arvore]),
                                              "Teste 7 - Testa a criação de novas linhas do mapa aleatoriamente com o numero 5" ~: Mapa 2 [(Estrada 1,[Nenhum,Tronco]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Nenhum])] ~=? estendeMapa (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva,[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum])]) 5,
                                              "Teste 8 - Testa a criação de novas linhas do mapa aleatoriamente com o numero 14" ~: Mapa 2 [(Rio 3,[Tronco,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Nenhum])] ~=? estendeMapa (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva,[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum])]) 14,
                                              "Teste 9 - Testa a criação de novas linhas do mapa aleatoriamente com o numero 41" ~: Mapa 2 [(Estrada 1,[Nenhum,Tronco]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Arvore]),(Relva,[Nenhum,Nenhum])] ~=? estendeMapa (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva,[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum])]) 41
                                              ]
                                              


 

