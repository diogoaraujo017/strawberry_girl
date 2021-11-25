module Fixtures where

import LI12122
import qualified Data.Functor as PROJETO

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

--MAPAS ADICIONADOS PELOS ALUNOS NESTE PROJETO.

m2 :: [(Peca, Coordenadas)]
m2 =
  [ (Bloco,(5,0)),(Bloco,(6,0)),(Bloco,(7,0)),(Bloco,(12,0)),(Bloco,(13,0)),
    (Bloco,(14,0)),(Bloco,(15,0)),(Bloco,(16,0)),(Bloco,(17,0)),(Bloco,(18,0)),
    (Bloco,(19,0)),(Bloco,(20,0)),(Bloco,(1,1)),(Bloco,(2,1)),(Bloco,(3,1)),
    (Bloco,(4,1)),(Bloco,(8,1)),(Bloco,(9,1)),(Bloco,(10,1)),(Bloco,(11,1)),
    (Bloco,(21,1)),(Bloco,(0,2)),(Bloco,(21,2)),(Bloco,(0,3)),(Bloco,(21,3)),
    (Bloco,(0,4)),(Bloco,(21,4)),(Bloco,(0,5)),(Bloco,(6,5)),(Bloco,(21,5)),
    (Bloco,(0,6)),(Bloco,(6,6)),(Bloco,(21,6)),(Bloco,(0,7)),(Bloco,(6,7)),
    (Caixa,(7,7)),(Caixa,(8,7)),(Caixa,(9,7)),(Caixa,(10,7)),(Bloco,(21,7)),
    (Bloco,(0,8)),(Porta,(1,8)),(Bloco,(5,8)),(Bloco,(6,8)),(Bloco,(7,8)),
    (Bloco,(8,8)),(Bloco,(9,8)),(Bloco,(10,8)),(Bloco,(11,8)),(Bloco,(21,8)),
    (Bloco,(0,9)),(Bloco,(1,9)),(Bloco,(3,9)),(Bloco,(4,9)),(Bloco,(5,9)),
    (Bloco,(11,9)),(Bloco,(12,9)),(Bloco,(14,9)),(Caixa,(20,9)),(Bloco,(21,9)),
    (Bloco,(1,10)),(Bloco,(3,10)),(Bloco,(12,10)),(Bloco,(14,10)),(Bloco,(15,10)),
    (Caixa,(19,10)),(Caixa,(20,10)),(Bloco,(21,10)),(Bloco,(1,11)),(Bloco,(3,11)),
    (Bloco,(12,11)),(Bloco,(14,11)),(Bloco,(15,11)),(Caixa,(18,11)),(Caixa,(19,11)),
    (Caixa,(20,11)),(Bloco,(21,11)),(Bloco,(1,12)),(Bloco,(2,12)),(Bloco,(3,12)),
    (Bloco,(12,12)),(Bloco,(14,12)),(Bloco,(15,12)),(Bloco,(16,12)),(Bloco,(17,12)),
    (Bloco,(18,12)),(Bloco,(19,12)),(Bloco,(20,12)),(Bloco,(21,12)),(Bloco,(12,13)),
    (Bloco,(13,13)),(Bloco,(14,13))
  ]
 

m2r :: Mapa
m2r =
  [ [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (12, 8) Oeste False)

m2e2 :: Jogo
m2e2 = Jogo m2r (Jogador (20,8) Este False)

m3 :: [(Peca, Coordenadas)]
m3 =
  [ (Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(5,1)),(Bloco,(6,1)),(Bloco,(7,1)),
    (Bloco,(8,1)),(Bloco,(9,1)),(Bloco,(10,1)),(Bloco,(11,1)),(Bloco,(12,1)),
    (Bloco,(13,1)),(Bloco,(14,1)),(Bloco,(15,1)),(Bloco,(16,1)),(Bloco,(17,1)),
    (Bloco,(0,2)),(Bloco,(2,2)),(Bloco,(4,2)),(Bloco,(18,2)),(Bloco,(0,3)),
    (Bloco,(3,3)),(Bloco,(18,3)),(Bloco,(0,4)),(Caixa,(17,4)),(Bloco,(18,4)),
    (Bloco,(0,5)),(Caixa,(16,5)),(Caixa,(17,5)),(Bloco,(18,5)),(Bloco,(0,6)),
    (Bloco,(2,6)),(Bloco,(3,6)),(Bloco,(4,6)),(Bloco,(13,6)),(Caixa,(14,6)),
    (Bloco,(16,6)),(Bloco,(17,6)),(Bloco,(0,7)),(Bloco,(2,7)),(Bloco,(4,7)),
    (Bloco,(9,7)),(Bloco,(12,7)),(Bloco,(13,7)),(Bloco,(14,7)),(Bloco,(15,7)),
    (Bloco,(16,7)),(Bloco,(0,8)),(Bloco,(2,8)),(Bloco,(4,8)),(Caixa,(5,8)),
    (Caixa,(6,8)),(Bloco,(8,8)),(Bloco,(9,8)),(Bloco,(12,8)),(Bloco,(0,9)),
    (Porta,(1,9)),(Bloco,(2,9)),(Bloco,(4,9)),(Bloco,(5,9)),(Bloco,(6,9)),
    (Bloco,(7,9)),(Bloco,(8,9)),(Bloco,(9,9)),(Bloco,(11,9)),(Bloco,(12,9)),
    (Bloco,(0,10)),(Bloco,(1,10)),(Bloco,(2,10)),(Bloco,(4,10)),(Bloco,(5,10)),
    (Bloco,(9,10)),(Bloco,(10,10)),(Bloco,(11,10))
  ]

m3r :: Mapa 
m3r = 
  [ [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

m3e1 :: Jogo 
m3e1 = Jogo m3r (Jogador (9,6) Oeste False)

m3e2 :: Jogo 
m3e2 = Jogo m3r (Jogador (17,3) Este False)



  