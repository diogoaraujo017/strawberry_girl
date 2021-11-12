{- |
Module      : Tarefa1_2021li1g097
Description : Validação de um potencial mapa
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g097 where

import LI12122

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
--validaPotencialMapa pecas = undefined
validaPotencialMapa = validaPosicao l


validaPosicao :: [(Peca, Coordenadas)] -> Bool
--validaPotencialMapa pecas = undefined
validaPosicao (p1,(x1,y1)):[] = True
validaPosicao (p1,(x1,y1)):(p2,(x2,y2)):t
 |x1==x2 && y1==y2 = False
 |otherwise = validaPosicao (p1(,x1,y1)):t