{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa3_2021li1g097
Description : Representação textual do jogo
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g097 where

import LI12122
import Tarefa2_2021li1g097

-- [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
-- mostrarJogo (Jogo [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]] (Jogador (2,5) Oeste False))

--TAREFA 3

{-
A função 'mostrarJogo' vai receber um jogo que consiste em um mapa e um jogador
e vai dar uma lista de strings que representão as linhas do jogo. Para percebemos
em que posição nos encontramos precisamos de um acomulador (xa,ya) mas, porque a 
função 'mostrarJogo' não recebe um par de Int para o acomulador, iremos usar uma
função auxiliar 'mostrarJogoAux'
-}
instance Show Jogo where

mostrarJogo :: Jogo -> [String]
mostrarJogo (Jogo m (Jogador c d b)) = mostrarJogoAux (0,0) (Jogo m (Jogador c d b))

{--}
mostrarJogoAux :: (Int,Int) -> Jogo -> [String]
mostrarJogoAux (_,_) (Jogo [] _) = []
mostrarJogoAux (x,y) (Jogo (m:ms) (Jogador c d b)) = descreveLinha (0,y) (Jogador c d b) m : mostrarJogoAux (0,y+1) (Jogo ms (Jogador c d b))

{--}
descreveLinha :: (Int,Int) -> Jogador -> [Peca] -> String
descreveLinha (_,_) (Jogador _ _ _) [] = ""
descreveLinha (x,y) (Jogador (x1,y1) d b) (Vazio:t)
 |x==x1 && y==y1 = (case d of Oeste -> "<"
                              Este  -> ">") ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
 |otherwise = " " ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
descreveLinha (x,y) (Jogador c d b) (a:t) = (case a of Bloco -> "X"
                                                       Porta -> "P"
                                                       Caixa -> "C") ++ descreveLinha (x+1,y) (Jogador c d b) t


--[(Bloco,(5,0)),(Bloco,(7,0)),(Bloco,(6,0)),(Bloco,(12,0)),(Bloco,(15,0)),(Bloco,(13,0)),(Bloco,(20,0)),(Bloco,(14,0)),(Bloco,(17,0)),(Bloco,(16,0)),(Bloco,(18,0)),(Bloco,(19,0)),(Bloco,(1,1)),(Bloco,(2,1)),(Bloco,(3,1)),(Bloco,(4,1)),(Bloco,(8,1)),(Bloco,(9,1)),(Bloco,(10,1)),(Bloco,(11,1)),(Bloco,(21,1)),(Bloco,(0,2)),(Bloco,(21,2)),(Bloco,(0,3)),(Bloco,(21,3)),(Bloco,(0,4)),(Bloco,(21,4)),(Bloco,(0,5)),(Bloco,(6,5)),(Bloco,(21,5)),(Bloco,(0,6)),(Bloco,(6,6)),(Bloco,(21,6)),(Bloco,(0,7)),(Bloco,(6,7)),(Bloco,(21,7)),(Caixa,(7,7)),(Caixa,(8,7)),(Caixa,(9,7)),(Caixa,(10,7)),(Bloco,(0,8)),(Porta,(1,8)),(Bloco,(5,8)),(Bloco,(6,8)),(Bloco,(7,8)),(Bloco,(8,8)),(Bloco,(9,8)),(Bloco,(10,8)),(Bloco,(11,8)),(Bloco,(0,9)),(Bloco,(1,9)),(Bloco,(3,9)),(Bloco,(4,9)),(Bloco,(5,9)),(Bloco,(11,9)),(Bloco,(12,9)),(Bloco,(14,9)),(Caixa,(20,9)),(Bloco,(21,9)),(Bloco,(1,10)),(Bloco,(3,10)),(Bloco,(12,10)),(Bloco,(14,10)),(Bloco,(15,10)),(Caixa,(19,10)),(Caixa,(20,10)),(Bloco,(21,10)),(Bloco,(1,11)),(Bloco,(3,11)),(Bloco,(12,11)),(Bloco,(14,11)),(Bloco,(15,11)),(Caixa,(18,11)),(Caixa,(19,11)),(Caixa,(20,11)),(Bloco,(21,11)),(Bloco,(1,12)),(Bloco,(2,12)),(Bloco,(3,12)),(Bloco,(12,12)),(Bloco,(14,12)),(Bloco,(15,12)),(Bloco,(16,12)),(Bloco,(17,12)),(Bloco,(18,12)),(Bloco,(19,12)),(Bloco,(20,12)),(Bloco,(21,12)),(Bloco,(12,13)),(Bloco,(13,13)),(Bloco,(14,13))]

--TAREFA CONCLUIDA !!!!!!







