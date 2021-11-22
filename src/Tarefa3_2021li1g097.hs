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

--TAREFA CONCLUIDA !!!!!!







