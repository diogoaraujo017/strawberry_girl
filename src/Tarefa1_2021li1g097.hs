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

{-Esta função testa se o mapa é valido utilizando as funções auxiliares que estão posteriormente defenidas.-} --Done
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa m = validaPosicao m && portaValida m && caixaValida m && existeVazio m && chaoContinuo m 


{-Esta função testa se existem peças que têm as mesmas coordenadas.-}  --Done
validaPosicao :: [(Peca, Coordenadas)] -> Bool
validaPosicao ((p1,(x1,y1)):[]) = True
validaPosicao ((p1,(x1,y1)):(p2,(x2,y2)):t)
 |x1==x2 && y1==y2 = False
 |otherwise = validaPosicao ((p1,(x1,y1)):t) && validaPosicao ((p2,(x2,y2)):t)

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


{-Esta função testa se as caixas estão propriamente colocadas no mapa, dando o valor de False se existirem caixas a flutuar.-}  --Done
caixaValida :: [(Peca, Coordenadas)] -> Bool
caixaValida [] = True
caixaValida ((Caixa, (x1,y1)):[]) = False
caixaValida ((Caixa, (x1,y1)):(p, (x2,y2)):t) = case p of Bloco -> (x1 == x2 && y1 == (y2-1)) || caixaValida ((Caixa, (x1,y1)):t)
                                                          Porta -> caixaValida ((Caixa, (x1,y1)):t)
                                                          Caixa -> if x1 == x2 && y1 == (y2-1) then caixaValida ((p, (x2,y2)):t) else caixaValida ((Caixa, (x1,y1)):t) && caixaValida ((p,(x2,y2)):t)
caixaValida ((p, (x,y)):t) = caixaValida t 


{-Esta função testa se existe pelo menos um espaço vazio no mapa.-} --Por fazer
{-existeVazio :: [(Peca, Coordenadas)] -> Bool
existeVazio ((p1, (x1,y1)):[]) = False
existeVazio ((p1, (x1,y1)):(p2, (x2,y2)):t) 
  |p1 == Vazio || p2 == Vazio = True
  |x1 == x2 && y1 == (y2+1) = existeVazio ((p2, (x2,y2)):t)
  |x2 == (x1+1) = existeVazio ((p2,(x2,y2)):t)
  |otherwise = True 
-}

{-Esta função testa se existe um chão continuo-}  --Por fazer 
chaoContinuo :: [(Peca, Coordenadas)] -> Bool 
-- chaoContinuo [] = True ?????
chaoContinuo = undefined












