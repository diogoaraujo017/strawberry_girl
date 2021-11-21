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

--[[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

instance Show Jogo where
  --show = undefined

mostrarJogo :: Mapa -> Jogador -> [String]
mostrarJogo [] (Jogador (x,y) d b)
  |d == Este = [">"]
  |otherwise = ["<"]
mostrarJogo m (Jogador (x,y) d b) = show1 (0,0) m (Jogador (x,y) d b)


show1 :: (Int,Int) -> Mapa -> Jogador -> [String]
show1 (x1,y1) ([]:t) j = show1 (x1,y1+1) t j
show1 (x1,y1) (h:t) (Jogador (x2,y2) d b)
 |x1==x2 && y1==y2 = if d==Este then ([">"] ++ show2 (h:t)) else ["<"] ++ show2 (h:t)
 |otherwise = show1 (x1+1,y1) ((tail h) :t) (Jogador (x2,y2) d b)

show2 :: Mapa -> [String]
show2 (h:t) = descreveLinha h : show2 t
  where descreveLinha :: [Peca] -> String
        descreveLinha [] = ""
        descreveLinha (a:b) = (case a of Bloco -> "X"
                                         Porta -> "P"
                                         Caixa -> "C"
                                         Vazio -> " ") ++ descreveLinha b


















{-

showJogo1 :: Jogo -> [String]
showJogo1 [] = []
showJogo1 m j = insereJogador j 




insereJogador :: Jogador -> Jogo
insereJogador (Jogador (x,y) d b) ( = 
insereJogador (Jogador (x,y) d b) 

-}