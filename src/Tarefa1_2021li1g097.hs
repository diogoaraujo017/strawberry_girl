{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- |
Module      : Tarefa1_2021li1g097
Description : Validação de um potencial mapa
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g097 where

import LI12122
import Language.Haskell.TH (charPrimL)
import Generics.SYB (ConstrRep(CharConstr))
--TAREFA 1
{-Esta função testa se o mapa é valido utilizando as funções auxiliares que estão posteriormente defenidas.-} --Done
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa m = validaPosicao m && portaValida m && caixaValida m && existeVazio m && chaoContinuo (maiorY m) m

--TAREFA 1.1---------------------------------------------------------------------------------------------------------------------------------------
{-Esta função testa se existem peças que têm as mesmas coordenadas.-}  --Done
validaPosicao :: [(Peca, Coordenadas)] -> Bool
validaPosicao [(p1,(x1,y1))] = True
validaPosicao l
 |x1==x2 && y1==y2 = False
 |otherwise = validaPosicao ((p2,(x2,y2)):t)
 where ((p1,(x1,y1)):(p2,(x2,y2)):t) = ordenaPecas l

--TAREFA 1.2---------------------------------------------------------------------------------------------------------------------------------------
{-Esta função testa se existe apenas uma porta usando a função existePorta.-}  --Done
portaValida :: [(Peca, Coordenadas)] -> Bool
portaValida m
  |existePorta m == 1 = True
  |otherwise = False

{-Esta função testa quantas portas existem.-} --Done
existePorta :: [(Peca, Coordenadas)] -> Int
existePorta [] = 0
existePorta ((Porta, (x,y)):t) = 1 + existePorta t
existePorta ((_, (x,y)):t) = existePorta t

--TAREFA 1.3---------------------------------------------------------------------------------------------------------------------------------------
{-Esta função testa se as caixas estão propriamente colocadas no mapa, dando o valor de False se existirem caixas a flutuar.-}  --Done
caixaValida :: [(Peca, Coordenadas)] -> Bool
caixaValida [] = True
caixaValida [(Caixa, (_,_))] = False
caixaValida l = if p1 == Caixa then case p2 of Bloco -> (x1 == x2 && y1 == (y2-1)) || caixaValida ((p1,(x1,y1)):t)
                                               Porta -> caixaValida ((Caixa, (x1,y1)):t)
                                               Caixa -> if x1 == x2 && y1 == (y2-1) then caixaValida ((p2, (x2,y2)):t) else caixaValida ((Caixa, (x1,y1)):t) && caixaValida ((p2,(x2,y2)):(p1, (x1,y1)):t)
                                               Vazio -> caixaValida ((Caixa,(x1,y1)):t)
                               else caixaValida ((p2,(x2,y2)):t)
--caixaValida ((_,(_,_)):t) = caixaValida t
 where ((p1,(x1,y1)):(p2,(x2,y2)):t) = ordenaPecas l


--TAREFA 1.4---------------------------------------------------------------------------------------------------------------------------------------
{-Esta função testa se existe pelo menos um espaço vazio no mapa.-} --buuugggg
existeVazio :: [(Peca, Coordenadas)] -> Bool
existeVazio [] = False
existeVazio l
  |p1 == Vazio = True
  |y1 == y2 && (abs(x1-x2)>1) = True
  |y1 /= y2 = existeVazio ((p2,(x2,y2)):t)
  where (p1,(x1,y1)):(p2,(x2,y2)):t = ordenaPecas l


{-Esta função ordena o mapa-} --Done
ordenaPecas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaPecas [] = []
ordenaPecas (h:t) = inserePeca h (ordenaPecas t)
    where inserePeca p [] = [p]
          inserePeca (p1,(x1,y1)) ((p2,(x2,y2)):z)
               |y1 > y2 || y1 == y2 && x1 > x2  = (p2,(x2,y2)):inserePeca (p1,(x1,y1)) z
               |y1 < y2 || y1 == y2 && x1 <= x2 = (p1,(x1,y1)):(p2,(x2,y2)):z

--TAREFA 1.5---------------------------------------------------------------------------------------------------------------------------------------
{-Esta função testa se existe um chão continuo-}  --Done
chaoContinuo :: (Peca,Coordenadas) -> [(Peca, Coordenadas)] -> Bool
chaoContinuo _ [] = True
chaoContinuo (p1,(x1,y1)) l
  |x1 == xMax z = True
  |elem (p1,(x1+1,y1)) z = chaoContinuo (p1,(x1+1,y1)) z
  |elem (p1,(x1,y1-1)) z = chaoContinuo (p1,(x1,y1-1)) z
  |elem (p1,(x1,y1+1)) z = chaoContinuo (p1,(x1,y1+1)) z
  |otherwise = False
  where z = ordenaPecas l

{-Esta função calcula o x maximo do mapa-}
xMax :: [(Peca, Coordenadas)] -> Int
xMax [] = undefined
xMax [(p1,(x1,y1))] = x1
xMax ((p1,(x1,y1)):(p2,(x2,y2)):t)
 |x1 >= x2 = xMax ((p1,(x1,y1)):t)
 |otherwise = xMax ((p2,(x2,y2)):t)


{-Esta função da output a primeira coluna-}
colunaUmBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
colunaUmBlocos [] = []
colunaUmBlocos l
 |(x1 == 0) && (p1 == Bloco) = (p1,(x1,y1)):colunaUmBlocos t
 |otherwise = colunaUmBlocos t
 where ((p1,(x1,y1)):t) = ordenaPecas l

{-Esta função encontra qual a ultima peça em x=1.-}
maiorY :: [(Peca, Coordenadas)] ->  (Peca, Coordenadas)
maiorY [h] = h
maiorY l
 |null t = undefined
 |otherwise = maiorY t
 where t = colunaUmBlocos l

--TAREFA CONCLUIDA!!