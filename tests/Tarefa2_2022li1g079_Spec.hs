module Tarefa2_2022li1g079_Spec where

import LI12223
import Tarefa2_2022li1g079
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1 - Testa Proximos Terrenos com quatro rios seguidos" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 2 [(Rio 0,[Nenhum,Nenhum]),(Rio 0,[Tronco,Nenhum]),(Rio 0, [Nenhum,Tronco]),(Rio 0, [Nenhum,Tronco])])

