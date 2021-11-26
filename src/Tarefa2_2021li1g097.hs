{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa2_2021li1g097
Description : Construção/Desconstrução do mapa
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g097 where

import LI12122
import Tarefa1_2021li1g097 (ordenaPecas,xMax)

{-Esta função pega numa lista de peças e coordenadas e tranforma essa lista num mapa.-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = mapaFinal z (colocaPeca (0,0) z z)
  where z = removePecasIguais l

{-Esta função pega numa lista de peças e coordenadas e retira uma das posições iguas num mapa, se existirem.-}
removePecasIguais :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removePecasIguais [] = []
removePecasIguais [(p,(x,y))] = [(p,(x,y))]
removePecasIguais l
 |p1 == p1 && x1==x2 && y1==y2 = removePecasIguais ((p1,(x1,y1)):t)
 |otherwise = (p1,(x1,y1)):removePecasIguais ((p2,(x2,y2)):t)
 where (p1,(x1,y1)):(p2,(x2,y2)):t = ordenaPecas l

{-Esta função recebe um par de inteiros que ditam a posição em que vamos e recebe uma 
lista de pecas e coordenadas e tranforma essa lista numa lista de peças em ordem com 
os respetivos vazios nas devidsa cordenadas.-}
colocaPeca :: (Int,Int) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [Peca]
colocaPeca (x,y) [] l
 |x > m = []
 |otherwise = Vazio:colocaPeca (x+1,y) [] l
 where m = xMax l
colocaPeca (x,y) l l1
 |x==x1 && y==y1 = p1:colocaPeca (x+1,y) t l1
 |x/=x1 && y==y1 = Vazio:colocaPeca (x+1,y) ((p1,(x1,y1)):t) l1
 |y/=y1 && x<xm  = Vazio:colocaPeca (x+1,y) ((p1,(x1,y1)):t) l1
 |y/=y1 && x==xm = Vazio:colocaPeca (0,y+1) ((p1,(x1,y1)):t) l1
 |y/=y1 && x>xm = colocaPeca (0,y+1) ((p1,(x1,y1)):t) l1
 where (p1,(x1,y1)):t = ordenaPecas l
       xm = xMax l1

{-Esta função tranforma uma lista peças no respetivo mapa.-}
mapaFinal :: [(Peca, Coordenadas)] -> [Peca] -> Mapa
mapaFinal l [] = []
mapaFinal l m = takePecas (xMax l+1) m:mapaFinal l (removePecas (xMax l+1) m)

{-Esta funçao obtem a primeira linha do mapa final atraves da lista de peças ordenadas
pela funçao colocaPecas-}
takePecas :: Int -> [Peca] -> [Peca]
takePecas _ [] = []
takePecas 0 l  = []
takePecas x (h:t) = h:takePecas (x-1) t

{-Esta funçao remove a primeira linha do mapa final atraves da lista de peças ordenadas 
pela funçao colocaPecas-}
removePecas :: Int -> [Peca] -> [Peca]
removePecas _ [] = []
removePecas 0 m = m
removePecas x (h:t) = removePecas (x-1) t

--TAREFA 2.2-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-Esta função pega num Mapa e tranforma esse mapa na lista das coordenadas ocultando os 
vazios-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa m = somaPos (0,0) m

{-Esta função pega num par de inteiros e num Mapa e tranforma esse mapa na lista das 
coordenadas começando na coordenada defenida no par de inteiros ocultando os vazios-}
somaPos :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)]
somaPos (_,_) [] = []
somaPos (x,y) ([]:t) = somaPos (0,y+1) t
somaPos (x,y) ((p:ps):t) = case p of Vazio -> somaPos (x+1,y) (ps:t)
                                     _ -> (p,(x,y)):somaPos (x+1,y) (ps:t)

{-Esta função pega num Mapa e tranforma esse mapa na lista das coordenadas sem ocultar os vazios-}
desconstroiMapa2 :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa2 [] = []
desconstroiMapa2 m = somaPos2 (0,0) m

{-Esta função pega num par de inteiros e num Mapa e tranforma esse mapa na lista das coordenadas começando na coordenada defenida no par de inteiros sem ocultar os vazios-}
somaPos2 :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)]
somaPos2 (_,_) [] = []
somaPos2 (x,y) ([]:t) = somaPos2 (0,y+1) t
somaPos2 (x,y) ((p:ps):t) = (p,(x,y)):somaPos2 (x+1,y) (ps:t)

--TAREFA CONCLUIDA !!!!!!
