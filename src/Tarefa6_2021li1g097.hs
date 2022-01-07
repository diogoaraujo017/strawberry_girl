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
import Tarefa4_2021li1g097 (correrMovimentos, moveJogador,verificaLados,verificaLadosCaixa,podePegar,podeLargar,podeTrepar,podeTreparCaixa,)
import Tarefa5_2021li1g097 (coordPorta, a)
import System.Random ( Random(randomIO), randomRIO )
import Text.Parsec.Combinator


--resolveJogo :: Int -> Jogo -> Maybe [Movimento]
--resolveJogo i (Jogo m (Jogador c d b)) = procuraPath (listaMovimentos i (movimentosPossiveis (Jogo m (Jogador c d b))) (Jogo m (Jogador c d b))) (Jogo m (Jogador c d b))


movimentosPossiveis :: Jogo -> [Movimento]
movimentosPossiveis (Jogo (p:ps) (Jogador (x,y) d b)) = if not b
                                                          then if not (estaPreso (0,0) (Jogo (p:ps) (Jogador (x,y) d b)))
                                                                      then if d == Oeste 
                                                                             then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita 
                                                                                      then [AndarDireita,AndarEsquerda] ++ if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                   then [Trepar] ++ if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                                      then [InterageCaixa]
                                                                                                                                                      else []
                                                                                                                                   else if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                             then [InterageCaixa]
                                                                                                                                             else []
                                                                                      else [AndarEsquerda] ++ if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                   then [Trepar] ++ if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                                      then [InterageCaixa]
                                                                                                                                                      else []
                                                                                                                                   else if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                             then [InterageCaixa]
                                                                                                                                             else []
                                                                             else if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda 
                                                                                      then [AndarDireita,AndarEsquerda]
                                                                                      else [AndarDireita] 
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           
                                                                                           --   then [AndarDireita] ++ if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                         then [Trepar] ++ if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                              then [InterageCaixa]
                                                                                                                                              else []
                                                                                                                         else if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                     then [InterageCaixa]
                                                                                                                                     else []
                                                                                              else if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                       then [Trepar] ++ if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                                                       else if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                      else []
                                                          else if not (estaPreso (0,0) (Jogo (p:ps) (Jogador (x,y) d b)))                        
                                               ------------------------------------------------------------------------------------------------------------------------------------------
                                                    {-     else if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                                                                  then [AndarEsquerda] ++ if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                                                                                    then [AndarDireita] ++ if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                              then [Trepar] ++ if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                                                              else if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                                    else if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                             then [Trepar] ++ if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                                             else if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                  else if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                                                                         then [AndarDireita] ++ if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                              then [Trepar] ++ if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                                                              else if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                                  then [InterageCaixa]
                                                                                                                                  else []
                                                                          else if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                then [Trepar] ++ if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                      then [InterageCaixa]
                                                                                                      else []
                                                                                else if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                      then [InterageCaixa]
                                                                                                      else []

                                                                                                      -}

listaMovimentos :: Int -> [Movimento] -> Jogo -> [[Movimento]]
listaMovimentos i [] j = []
listaMovimentos 0 m j = []
listaMovimentos i (m:mv) j = map (m :) (listaMovimentos (i - 1) (movimentosPossiveis (moveJogador j m)) (moveJogador j m)) ++ listaMovimentos i mv j

randomElem :: [a] -> IO a
randomElem l = do x <- randomRIO (0,length l - 1)
                  return (l !! x)

removeMov :: Movimento -> [Movimento] -> [Movimento]
removeMov m [] = []
removeMov m (x:xs) = if m == x then xs else x : (removeMov m xs)

procuraPath :: [[Movimento]] -> Jogo -> Maybe [Movimento]
procuraPath [] j = Nothing
procuraPath (x:xs) j
 |coordPorta2 j x = Just x
 |otherwise = procuraPath xs j

coordPorta2 :: Jogo -> [Movimento] -> Bool
coordPorta2 j m = (coordPorta (0,0) (correrMovimentos j m))

estaPreso :: (Int,Int) -> Jogo ->  Bool
estaPreso (x1,y1) (Jogo ([]:ps) (Jogador (x,y) d b)) = estaPreso (0,y1+1) (Jogo ps (Jogador (x,y) d b))
estaPreso (x1,y1) (Jogo ((h:t):ps) (Jogador (x,y) d b)) 
 |x1 == x-1 && y1 == y-1 = h == Bloco || h == Caixa 
 |x1 == x+1 && y1 == y-1 = h == Bloco || h == Caixa 
 |x1 == x-1 && y1 == y = h == Bloco || h == Caixa 
 |x1 == x+1 && y1 == y = h == Bloco || h == Caixa 
 |otherwise = estaPreso (x1+1,y1) (Jogo (t:ps) (Jogador (x,y) d b)) 

estaEntre :: (Int,Int) -> Jogo ->  Bool
estaEntre (x1,y1) (Jogo ([]:ps) (Jogador (x,y) d b)) = estaEntre (0,y1+1) (Jogo ps (Jogador (x,y) d b))
estaEntre (x1,y1) (Jogo ((h:t):ps) (Jogador (x,y) d b))  
 |x1 == x-1 && y1 == y = h == Bloco || h == Caixa 
 |x1 == x+1 && y1 == y = h == Bloco || h == Caixa 
 |otherwise = estaEntre (x1+1,y1) (Jogo (t:ps) (Jogador (x,y) d b)) 

