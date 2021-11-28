module Tarefa1_2021li1g097_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g097
import Fixtures


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
    , "Tarefa 1 - Teste Valida Mapa sem um chao continuo" ~: validaPotencialMapa [(Porta, (0, 2)),(Bloco, (0, 3)),(Bloco, (1, 3)),(Bloco, (3, 3)),(Caixa, (4, 2)),(Bloco, (4, 4)),(Bloco, (5, 4)),(Bloco, (6, 0)),(Bloco, (6, 1)),(Bloco, (6, 2)),(Bloco,(6,3))] ~=? False 
    , "Tarefa 1 - Teste Valida Mapa com mapa valido com vazios indicados" ~: validaPotencialMapa [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(3,0)),(Vazio,(4,0)),(Vazio,(5,0)),(Bloco,(6,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Vazio,(3,1)),(Vazio,(4,1)),(Vazio,(5,1)),(Bloco,(6,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Vazio,(3,2)),(Caixa,(4,2)),(Vazio,(5,2)),(Bloco,(6,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3)),(Bloco,(4,3)),(Bloco,(5,3)),(Bloco,(6,3))] ~=? True
    ]