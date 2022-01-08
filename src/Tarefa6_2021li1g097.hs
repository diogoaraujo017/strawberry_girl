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


movimentosPossiveis :: Jogo -> [Movimento]
movimentosPossiveis (Jogo (p : ps) (Jogador (x, y) d b))
  | not b = if not (estaPreso (0,0) (Jogo (p:ps) (Jogador (x,y) d b)))
                   then if d == Oeste
                          then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                                   then [AndarDireita,AndarEsquerda] ++ if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                then Trepar : [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                                else [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                   else AndarDireita : if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                             then Trepar : [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                             else [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                          else if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                                   then [AndarDireita,AndarEsquerda] ++ if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                then Trepar : [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                                else [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                   else AndarEsquerda : if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                             then Trepar : [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                             else [InterageCaixa | podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                   else []
  | otherwise = if not (estaPreso (0,0) (Jogo (p:ps) (Jogador (x,y) d b)))  
                                                     then if d == Oeste
                                                                    then if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                                                                             then [AndarDireita,AndarEsquerda] ++ if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                          then Trepar : [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                                                                          else [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                             else AndarDireita : if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                       then Trepar : [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                                                                       else [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                    else if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                                                                             then [AndarDireita,AndarEsquerda] ++ if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                          then Trepar : [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                                                                          else [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                             else AndarEsquerda : if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                                                                                                       then Trepar : [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                                                                                       else [InterageCaixa | podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))]
                                                     else []


separaListas :: [Movimento] -> [[Movimento]]
separaListas = map (: [])


constroiListasMov :: [Movimento]      -- lista que queresmos aumentar
                    -> [Movimento]   -- movimentos possiveis
                    -> [[Movimento]]
constroiListasMov l = map (\ y -> l ++ [y])

----
dropMov :: Movimento -> [Movimento] -> [Movimento]
dropMov m [] = []
dropMov m (x:xs) = if elem m (x:xs) then if m == x
                                            then xs
                                            else dropMov m xs 
                                    else x:xs


removeMovimentos :: [Movimento] -> [Movimento] -> Jogo -> [Movimento] 
removeMovimentos l [] _ = []
removeMovimentos l m (Jogo (p : ps) (Jogador (x, y) d b)) = if (!!) l ((length l)-1) == Trepar 
                                                                    then if d == Oeste
                                                                             then dropMov AndarDireita m
                                                                             else dropMov AndarEsquerda m
                                                                    else if length l >= 3
                                                                              then if (!!) l ((length l)-1) == AndarEsquerda && (!!) l ((length l)-2) == AndarEsquerda && (!!) l ((length l)-3) == AndarEsquerda
                                                                                    then dropMov AndarDireita m
                                                                                    else if (!!) l ((length l)-1) == AndarDireita && (!!) l ((length l)-1) == AndarDireita && (!!) l ((length l)-3) == AndarDireita 
                                                                                             then dropMov AndarEsquerda m
                                                                                             else m
                                                                              else if (!!) l ((length l)-1) == InterageCaixa 
                                                                                        then dropMov InterageCaixa m
                                                                                        else if length l >= 2
                                                                                                  then if (!!) l ((length l)-1) == AndarDireita  && (!!) l ((length l)-2) == AndarEsquerda
                                                                                                     then dropMov AndarDireita m
                                                                                                     else m
                                                                                                  else if (!!) l ((length l)-1) == AndarEsquerda  && (!!) l ((length l)-2) == AndarDireita
                                                                                                            then dropMov AndarEsquerda m
                                                                                                            else m
 
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i jogo
 |i <= 0 = Nothing
 |otherwise = procuraPath (combina i (separaListas (movimentosPossiveis jogo)) jogo) jogo

combina :: Int -> [[Movimento]] -> Jogo -> [[Movimento]]
combina 0 m _ = m
combina _ [] _ = []
combina i (x:xs) jogo = combina (i-1) (constroiListasMov x (removeMovimentos x (movimentosPossiveis (correrMovimentos jogo x )) (correrMovimentos jogo x ))) jogo ++ resto
 where resto = combina i xs jogo

procuraPath :: [[Movimento]] -> Jogo -> Maybe [Movimento]
procuraPath [] j = Nothing
procuraPath (x:xs) j
 |coordPorta (0,0) (correrMovimentos j x) = Just x
 |otherwise = procuraPath xs j

estaPreso :: (Int,Int) -> Jogo ->  Bool
estaPreso (x1,y1) (Jogo ([]:ps) (Jogador (x,y) d b)) = estaPreso (0,y1+1) (Jogo ps (Jogador (x,y) d b))
estaPreso (x1,y1) (Jogo ((h:t):ps) (Jogador (x,y) d b))
 |x1 == x-1 && y1 == y-1 = h == Bloco || h == Caixa
 |x1 == x+1 && y1 == y-1 = h == Bloco || h == Caixa
 |x1 == x-1 && y1 == y = h == Bloco || h == Caixa
 |x1 == x+1 && y1 == y = h == Bloco || h == Caixa
 |otherwise = estaPreso (x1+1,y1) (Jogo (t:ps) (Jogador (x,y) d b))



