{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa6_2021li1g097
Description : Resolução de um puzzle
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}
module Tarefa6_2021li1gXXX where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI12122
import Tarefa1_2021li1g097 () 
import Tarefa2_2021li1g097 ()
import Tarefa3_2021li1g097 ()
import Tarefa4_2021li1g097 () 

resolveJogo :: Int -> Jogo -> Maybe [Movimento]
--resolveJogo i jogo = undefined
resolveJogo i (Jogo (p:ps) (Jogador (x,y) d b))
 |i==0 = if x == fst (cordPorta (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) && y == snd (cordPorta (0,0) (Jogo (p:ps) (Jogador (x,y) d b)))
         then Just []
         else Nothing
resolveJogo i (Jogo (p:ps) (Jogador (x,y) d b)) = AndarEsquerda : resolveJogo i-1 (Jogo (p:ps) (Jogador (x-1,y) d b)) || AndarDireita : resolveJogo i-1 (Jogo (p:ps) (Jogador (x+1,y) d b)) || if d==oeste then Trepar : resolveJogo i-1 (Jogo (p:ps) (Jogador (x-1,y+1) d b)) else Trepar : resolveJogo i-1 (Jogo (p:ps) (Jogador (x+1,y+1) d b)) || InterageCaixa : resolveJogo i-1 (Jogo (p:ps) (Jogador (x,y) d b))


cordPorta :: (Int,Int) -> Jogo -> (Int,Int)
cordPorta (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = lPorta (0,0) p (Jogador (x,y) d b)
                                                     where lPorta :: (Int,Int) -> [Peca] -> Jogador -> (Int,Int)
                                                           lPorta (a1,a2) [] (Jogador (x,y) d b) = cordPorta (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                           lPorta (a1,a2) (h:t) (Jogador (x,y) d b) = case h of Porta -> (a1,a2)
                                                                                                                Bloco -> lPorta (a1+1,a2) t (Jogador (x,y) d b)
                                                                                                                Caixa -> lPorta (a1+1,a2) t (Jogador (x,y) d b)
                                                                                                                Vazio -> lPorta (a1+1,a2) t (Jogador (x,y) d b)



