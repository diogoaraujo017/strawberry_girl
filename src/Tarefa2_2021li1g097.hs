{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{- |
Module      : Tarefa2_2021li1g097
Description : Construção/Desconstrução do mapa
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g097 where

import LI12122
import GHC.RTS.Flags (ProfFlags(includeTSOs))

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa = undefined

{-
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa m = constroiMapaAux m 0


constroiMapaAuxX :: ord Coordenadas => [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
constroiMapaAuxX (m:ms)= subCNAX m 


constroiMapaAuxY :: [(Peca, Coordenadas)] -> Int -> [[(Peca, Coordenadas)]]
constroiMapaAuxY ((p,(x,y)):t) n
  |n>yMax (1,0) ((p,(x,y)):t) = [] 
  |((p,(x,y)):t)==[] = constroiMapaAuxY ((p,(x,y)):t) n+1
  |otherwise = if n==y then (p,(x,y)) : constroiMapaAuxY t n else constroiMapaAuxY t n

--[(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]

xMax :: (Int,Int) -> [(Peca, Coordenadas)] -> Int
xMax (pa,xm) [] = xm
xMax (pa,xm) ((,(x,)):t)
  |xm >= x = xMax (pa+1,xm) t
  |otherwise = xMax (pa+1,x) t

yMax :: (Int,Int) -> [(Peca, Coordenadas)] -> Int
yMax (pa,ym) [] = ym
yMax (pa,ym) ((,(,y)):t)
  |ym >= y = yMax (pa+1,ym) t
  |otherwise = yMax (pa+1,y) t
  
  -}

{-
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa m = posicao ((0,0) m) 
desconstroiMapa [] = []


posicao :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)]
posicao (x,y) ((p:t1):t2) = case p of Vazio -> posicao ((x+1,y) t1)
                                      Bloco -> (Bloco, (x,y)):posicao ((x+1,y) t1)
                                      Porta -> (Porta, (x,y)):posicao ((x+1,y) t1)
                                      Caixa -> (Caixa, (x,y)):posicao ((x+1,y) t1)
posicao (x,y) ([]:t1) = posicao ((0,y+1) (head t1)
posicao (_,_) [] = []

-}



