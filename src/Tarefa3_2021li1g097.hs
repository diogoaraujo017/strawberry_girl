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
mostrarJogo (Jogo m (Jogador cord direc b)) = mostrarJogoAux (0,0) (Jogo m (Jogador cord direc b))

{-

-}
mostrarJogoAux :: (Int,Int) -> Jogo -> [String]
mostrarJogoAux (_,_) (Jogo [] _) = []
mostrarJogoAux (xa,ya) (Jogo (m:ms) (Jogador cord direc b)) = descreveLinha (0,ya) (Jogador cord direc b) m : mostrarJogoAux (0,ya+1) (Jogo ms (Jogador cord direc b))
                                                            where descreveLinha :: (Int,Int) -> Jogador -> [Peca] -> String
                                                                  descreveLinha (xad,yad) (Jogador cord direc b) [] = ""
                                                                  descreveLinha (xad,yad) (Jogador cord direc b) (Vazio:t)
                                                                   |xad==fst cord && yad==snd cord = (case direc of Oeste -> "<"
                                                                                                                    Este -> ">") ++ descreveLinha (xad+1,yad) (Jogador cord direc b) t
                                                                   |otherwise = " " ++ descreveLinha (xad+1,yad) (Jogador cord direc b) t
                                                                  descreveLinha (xad,yad) (Jogador cord direc b) (a:t) = (case a of Bloco -> "X"
                                                                                                                                    Porta -> "P"
                                                                                                                                    Caixa -> "C") ++ descreveLinha (xad+1,yad) (Jogador cord direc b) t
                                                                                                                                    