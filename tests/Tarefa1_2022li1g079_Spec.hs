module Tarefa1_2022li1g079_Spec where

import LI12223
import Tarefa1_2022li1g079
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Teste 1" ~: 1 ~=? 1]
