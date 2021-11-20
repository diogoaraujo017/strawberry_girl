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
import GHC.RTS.Flags (ProfFlags(includeTSOs), MiscFlags (machineReadable), ParFlags (parGcLoadBalancingEnabled))

constroiMapa :: [(Peca,Coordenadas)] -> Mapa 
constroiMapa [] = []
constroiMapa l = undefined

{-
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa ((p,(x,y)):[]) = [[p]]
constroiMapa p = (firstLine p):constroiMapa (proximoY p)

proximoY :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
proximoY [] = []
proximoY l
 |y1 == y2  = proximoY ((p2,(x2,y2)):t)
 |otherwise = (p2,(x2,y2)):t
 where (p1,(x1,y1)):(p2,(x2,y2)):t = ordenaPecas l

{-Esta função da output a primeira linha do nosso mapa-}
firstLine :: [(Peca, Coordenadas)] -> [Peca]
firstLine [] = []
firstLine l
   |y1 == y2 && x2 == x1+1 = p1:firstLine ((p2,(x2,y2)):t)
   |y1 == y2 && x2 /= x1+1 = [p1] ++ [Vazio] ++ firstLine ((Vazio,(x1+1,y1)):(p2,(x2,y2)):t)
   |y1 /= y2 && length (firstLine1 l) == 1 = somaVazios (x1+1) ++ [p1]
   |y1 /= y2 = [p1]
   where (p1,(x1,y1)):(p2,(x2,y2)):t = ordenaPecas l

{-Da-nos a primeira linha sem os vazios.-}
firstLine1 :: [(Peca, Coordenadas)] -> [Peca]
firstLine1 [] = []
firstLine1 [(p,(x,y))] = [p] 
firstLine1 l
   |y1 == y2 = p1:firstLine1 ((p2,(x2,y2)):t)
   |y1 /= y2 = [p1]
   where (p1,(x1,y1)):(p2,(x2,y2)):t = ordenaPecas l

{-Esta função da output a lista de vazios antes de um bloco nume linha.-}
somaVazios :: Int -> [Peca]
somaVazios 1 = []
somaVazios n = somaVazios (n-1) ++ [Vazio]

{-Esta função ordena o mapa-} --Done
ordenaPecas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaPecas [] = []
ordenaPecas (h:t) = inserePeca h (ordenaPecas t)
    where inserePeca p [] = [p]
          inserePeca (p1,(x1,y1)) ((p2,(x2,y2)):z)
               |y1 > y2 || y1 == y2 && x1 > x2  = (p2,(x2,y2)):inserePeca (p1,(x1,y1)) z
               |y1 < y2 || y1 == y2 && x1 <= x2 = (p1,(x1,y1)):(p2,(x2,y2)):z
-}
--[(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]


--TAREFA 2.2-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-Esta função pega num Mapa e tranforma esse mapa na lista das coordenadas ocultando os vazios-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)] 
desconstroiMapa [] = []
desconstroiMapa m = somaPos (0,0) m

{-Esta função pega num par de inteiros e num Mapa e tranforma esse mapa na lista das coordenadas começando na coordenada defenida no par de inteiros ocultando os vazios-}
somaPos :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)] 
somaPos (_,_) [] = []
somaPos (x,y) ([]:t) = somaPos (0,y+1) t 
somaPos (x,y) ((p:ps):t) = case p of Vazio -> somaPos (x+1,y) ((ps):t)
                                     _ -> (p,(x,y)):somaPos (x+1,y) ((ps):t)

{-Esta função pega num Mapa e tranforma esse mapa na lista das coordenadas sem ocultar os vazios-}
desconstroiMapa2 :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa2 [] = []
desconstroiMapa2 m = somaPos (0,0) m

{-Esta função pega num par de inteiros e num Mapa e tranforma esse mapa na lista das coordenadas começando na coordenada defenida no par de inteiros sem ocultar os vazios-}
somaPos2 :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)] 
somaPos2 (_,_) [] = []
somaPos2 (x,y) ([]:t) = somaPos (0,y+1) t 
somaPos2 (x,y) ((p:ps):t) = (p,(x,y)):somaPos (x+1,y) ((ps):t)

