module Tarefa1_2021li1g097_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g097
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com 0 portas" ~: validaPotencialMapa [(Bloco, (0,0)), (Bloco, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com 2 peças com as mesmas coordenadas" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa sem vazios" ~: validaPotencialMapa [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(1,2)),(Bloco,(2,0)),(Bloco,(2,1)),(Bloco,(2,2))]  ~=?  False
    , "Tarefa 1 - Teste Valida Mapa m2r" ~: validaPotencialMapa m2 ~=? True
    , "Tarefa 1 - Teste Valida Mapa m3r" ~: validaPotencialMapa m3 ~=? True
    , "Tarefa 1 - Teste Valida Mapa com uma caixa invalida" ~: validaPotencialMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa sem um chao continuo" ~: validaPotencialMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1))] ~=? False 
    ]

--TESTE TODOS ADICIONADOS!!


