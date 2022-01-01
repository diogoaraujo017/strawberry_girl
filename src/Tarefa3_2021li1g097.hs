{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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

{- | A função 'show' vai receber um jogo que consiste em um mapa e um jogador
e vai dar uma lista de listas de strings que representão as linhas do jogo. Para percebemos
em que posição nos encontramos precisamos de um acomulador do tipo (Int,Int).Como a 
função 'show' não recebe um par de inteiros para o acomulador, iremos usar uma
função auxiliar 'mostrarJogoAux'.

== Exemplos de utilização:

>>> show (Jogo [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))] (Jogador (6, 0) Oeste False))
"      <\n      X\n      X\nP   C X\nXXXXXXX"

== Código:
@
instance Show Jogo where
  show (Jogo m (Jogador c d b)) = 'mostrarJogoAux' (0,0) (Jogo m (Jogador c d b))
@
-}
instance Show Jogo where
  show (Jogo m (Jogador c d b)) = mostrarJogoAux (0,0) (Jogo m (Jogador c d b))

{- | A função 'mostrarJogoAux' converte a primeira linha num string através da função 'descreveLinha', 
  depois a essa string adiciona o caracter especial "\n" se a linha não for a última.
  Depois faz a recursividade da mesma (se ainda houver linhas) que visa colocar as linhas restantes em forma de string construindo
  uma string com parágrafos que indicam as mudanças de linha.

== Exemplos de utilização:
>>> mostrarJogoAux (0,0) (Jogo [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))] (Jogador (6, 0) Oeste False))
"      <\n      X\n      X\nP   C X\nXXXXXXX"

== Código:
@
mostrarJogoAux :: (Int,Int) -> Jogo -> String
mostrarJogoAux (_,_) (Jogo [] _) = []
mostrarJogoAux (x,y) (Jogo (m:ms) (Jogador c d b)) = descreveLinha (0,y) (Jogador c d b) m ++ if null (mostrarJogoAux (0,y+1) (Jogo ms (Jogador c d b)))
                                                                                                     then []
                                                                                                     else "\n" ++ mostrarJogoAux (0,y+1) (Jogo ms (Jogador c d b))
@
-}
mostrarJogoAux :: (Int,Int)      -- ^Acumulador que nos permite saber quando colocamos um parágrafo
                  -> Jogo        -- ^Jogo 
                  -> String      -- ^Resultado
mostrarJogoAux (_,_) (Jogo [] _) = []
mostrarJogoAux (x,y) (Jogo (m:ms) (Jogador c d b)) = descreveLinha (0,y) (Jogador c d b) m ++ if null (mostrarJogoAux (0,y+1) (Jogo ms (Jogador c d b)))
                                                                                                     then []
                                                                                                     else "\n" ++ mostrarJogoAux (0,y+1) (Jogo ms (Jogador c d b))

{- | A função 'descreverLinha' descreve uma linha de um jogo onde o jogador tem o valor de "<" (se estiver virado para Oeste) ou ">" 
 (se estiver virado para Este), as caixas o valor de "X", a porta de "P" e as caixas de "C".

 Esta função utiliza um acumulador para ter a informação de quando colocar o jogador, pois compara as coordenadas do acumulador 
 com as coordenadas do jogador, sendo assim capaz de o colocar de uma forma certa.

 Se a posição do jogador coincidir com a posição da porta temos que nessa posição ira ficar o jogador.

== Exemplos de utilização:

>>> descreverLinha

== Código:
@
descreveLinha :: (Int,Int) -> Jogador -> [Peca] -> String
descreveLinha (_,_) Jogador {} [] = ""
descreveLinha (x,y) (Jogador (x1,y1) d b) (Vazio:t)
 |x==x1 && y==y1 = (case d of Oeste -> "<"
                              Este  -> ">") ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
 |otherwise = " " ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
descreveLinha (x,y) (Jogador (x1,y1) d b) (Porta:t)
 |x==x1 && y==y1 = (case d of Oeste -> "<"
                              Este  -> ">") ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
 |otherwise = "P" ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
descreveLinha (x,y) (Jogador c d b) (a:t) = (case a of Bloco -> "X"
                                                       Porta -> "P"
                                                       Caixa -> "C") ++ descreveLinha (x+1,y) (Jogador c d b) t
@
-}
descreveLinha :: (Int,Int)       -- ^Acumulador para assegurar a informação da coordenada em que nos encontramos
                 -> Jogador      -- ^Informações sobre o jogador
                 -> [Peca]       -- ^Lista de peças que vamos percorrer
                 -> String       -- ^Resultado
descreveLinha (_,_) Jogador {} [] = ""
descreveLinha (x,y) (Jogador (x1,y1) d b) (Vazio:t)
 |x==x1 && y==y1 = (case d of Oeste -> "<"
                              Este  -> ">") ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
 |otherwise = " " ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
descreveLinha (x,y) (Jogador (x1,y1) d b) (Porta:t)
 |x==x1 && y==y1 = (case d of Oeste -> "<"
                              Este  -> ">") ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
 |otherwise = "P" ++ descreveLinha (x+1,y) (Jogador (x1,y1) d b) t
descreveLinha (x,y) (Jogador c d b) (a:t) = (case a of Bloco -> "X"
                                                       Porta -> "P"
                                                       Caixa -> "C") ++ descreveLinha (x+1,y) (Jogador c d b) t


