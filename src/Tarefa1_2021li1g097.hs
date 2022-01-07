{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

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


{- | A função 'validaPotencialMapa' testa se o mapa é válido 
  utilizando as funções auxiliares ('validaPosicao','portaValida','caixaValida',
  'existeVazio','chaoContinuo','xMax' e 'maiorY') que estão posteriormente defenidas
  ,que verificam todos os parâmetros a que uma mapa deve obdecer
  para que se diga uma mapa válido.

== Exemplos de utilização:

>>> validaPotencialMapa [(Porta, (0, 2)),(Bloco, (0, 3)), (Bloco, (1, 3)), (Bloco, (2, 3))]
True
>>> validaPotencialMapa [(Bloco, (0, 3)), (Bloco, (1, 3)), (Bloco, (1, 3))]
False

== Código:
@
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa m = 'validaPosicao' m && 'portaValida' m && 'caixaValida' m && 'existeVazio' m && 'chaoContinuo' ('xMax' m) ('maiorY' m) m
@
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa m = validaPosicao m && portaValida m && caixaValida m && existeVazio m && chaoContinuo (xMax m) (maiorY m) m

{- | A função 'validaPosicao' testa se existem peças que têm as mesmas coordenadas.
  Se existirem coordenadas iguais o mapa diz-se inválido pelo que a função retorna o 
  valor False.
  
  Esta função utiliza a função 'ordenaPecas', posteriormente defenida.

== Exemplos de utilização:

>>> validaPosicão [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
True
>>> validaPosicao [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(0,3))]
False

== Código:
@
validaPosicao [(Peca, Coordenadas)] -> Bool
validaPosicao [(p1,(x1,y1))] = True
validaPosicao l
  |x1==x2 && y1==y2 = False
  |x1<0 || y1<0 = False
  |otherwise = 'validaPosicao' ((p2,(x2,y2)):t)
 where ((p1,(x1,y1)):(p2,(x2,y2)):t) = 'ordenaPecas' l
@
-}
validaPosicao :: [(Peca, Coordenadas)] -- ^Assume-se que nunca recebe lista vazia
                  -> Bool              -- ^Resultado
validaPosicao [(p1,(x1,y1))] = True
validaPosicao l
 |x1==x2 && y1==y2 = False
 |x1<0 || y1<0 = False
 |otherwise = validaPosicao ((p2,(x2,y2)):t)
 where ((p1,(x1,y1)):(p2,(x2,y2)):t) = ordenaPecas l

{- | A função 'portaValida' testa se existe apenas uma porta usando a função 
  'existePorta' posteriormente defenida.

== Exemplos de utilização:

>>> portaValida [(Porta, (0, 2)),(Bloco, (0, 3)), (Bloco, (1, 3)), (Bloco, (2, 3))]
True
>>> portaValida [(Bloco, (0, 3)), (Bloco, (1, 3)), (Bloco, (2, 3))]
False

== Código:
@
portaValida :: [(Peca, Coordenadas)] -> Bool
portaValida m = 'existePorta' m == 1 
@
-}
portaValida :: [(Peca, Coordenadas)] -- ^Assume-se que nunca recebe uma lista vazia
               -> Bool               -- ^Resultado
portaValida m = existePorta m == 1

{- | A função 'existePorta' conta quantas portas existe
  num mapa.

== Exemplos de utilização:

>>> existePorta [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
1
>>> existePorta [(Porta, (0,2)),(Porta, (1,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,3))]
2

== Código:
@
existePorta :: [(Peca, Coordenadas)] -> Int
existePorta [] = 0
existePorta ((Porta, (x,y)):t) = 1 + existePorta t
existePorta ((_, (x,y)):t) = existePorta t
@
-}
existePorta :: [(Peca, Coordenadas)] -> Int
existePorta [] = 0
existePorta ((Porta, (x,y)):t) = 1 + existePorta t
existePorta ((_, (x,y)):t) = existePorta t

{- | A função 'caixaValida' verifica se as caixas presentes
  num mapa se encontram bem defenidas, ou seja, esta função 
  vai verificar se as caixas presentes no mapa têm um bloco, uma porta
  ou uma caixa debaixo das mesmas, dando output False se existirem 
  caixas a flutuar.
  
  Esta função utiliza a função 'ordenaPecas' 
  posteriormente defenida, para ordenar o mapa.

== Exemplos de utilização:

>>> caixaValida [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Caixa,(1,2))]
True
>>> caixaValida [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco,(2,3)),(Caixa, (2,1))]
False

== Código:
@
caixaValida :: [(Peca,Coordenadas)] -> Bool
caixaValida [] = True
caixaValida [(Caixa, (_,_))] = False
caixaValida p = if p1 == Caixa then (elem (Caixa,(x1,y1+1)) t || elem (Bloco,(x1,y1+1)) t || elem (Porta,(x1,y1+1)) t) && caixaValida t
                               else caixaValida t
  where ((p1,(x1,y1)):t) = ordenaPecas p
@
-}
caixaValida :: [(Peca,Coordenadas)] -> Bool
caixaValida [] = True
caixaValida [(Caixa, (_,_))] = False
caixaValida p = if p1 == Caixa then (elem (Caixa,(x1,y1+1)) t || elem (Bloco,(x1,y1+1)) t || elem (Porta,(x1,y1+1)) t) && caixaValida t
                               else caixaValida t
  where ((p1,(x1,y1)):t) = ordenaPecas p

{- | A função 'existeVazio' verifica se existe, pelo menos, um
  vazio não declarado (verificando se o mapa tem menos elementos
  do que a matriz de ordem m (maior y do mapa encontrado pela função
  'yMax' adicionando 1) por n (maior x do mapa encontrado pela função
  'xMax' adicionando 1)) ou um vazio declarado (através da função 'existeVazio1').

== Exemplos de utilização:

>>> existeVazio [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
True
>>> existeVazio [(Bloco,(0,0)),(Bloco,(0,1)),(Bloco,(0,2)),(Bloco,(1,0)),(Bloco,(1,1)),(Bloco,(1,2)),(Bloco,(2,0)),(Bloco,(2,1)),(Bloco,(2,2))]
False

== Código:
@
existeVazio :: [(Peca, Coordenadas)] -> Bool
existeVazio [] = False
existeVazio ((p,(x,y)):t) = length z /= (xMax z + 1)*(yMax z + 1) || p == Vazio || existeVazio1 t
 where z = (p,(x,y)):t
@
-}
existeVazio :: [(Peca, Coordenadas)] -> Bool
existeVazio [] = False
existeVazio ((p,(x,y)):t) = length z /= (xMax z + 1)*(yMax z + 1) || p == Vazio || existeVazio1 t
 where z = (p,(x,y)):t

{- | A função 'existeVazio1' verifica se existe pelo menos
  um vazio declarado.

== Exemplos de utilização:

>>> existeVazio1 [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
False
>>> existeVazio1 [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Vazio,(1,2))]
True

== Código:
@
existeVazio1 :: [(Peca, Coordenadas)] -> Bool
existeVazio1 [] = False
existeVazio1 ((Vazio,(x,y)):t) = True 
existeVazio1 ((_,(_,_)):t) = existeVazio1 t
@
-}
existeVazio1 :: [(Peca, Coordenadas)] -> Bool
existeVazio1 [] = False
existeVazio1 ((Vazio,(x,y)):t) = True
existeVazio1 ((_,(_,_)):t) = existeVazio1 t

{- | A função 'yMax' calcula o maior y
 do mapa

== Exemplos de utilização:

>>> yMax[(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
3
>>> yMax [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,4))]
4

== Código:
@
yMax :: [(Peca, Coordenadas)] -> Int
yMax [] = 0
yMax [(p1,(x1,y1))] = y1
yMax ((p1,(x1,y1)):(p2,(x2,y2)):t)
 |y1 >= y2 = yMax ((p1,(x1,y1)):t)
 |otherwise = yMax ((p2,(x2,y2)):t)
@ 
-}
yMax :: [(Peca, Coordenadas)] -> Int
yMax [] = 0
yMax [(p1,(x1,y1))] = y1
yMax ((p1,(x1,y1)):(p2,(x2,y2)):t)
 |y1 >= y2 = yMax ((p1,(x1,y1)):t)
 |otherwise = yMax ((p2,(x2,y2)):t)

{- | A função 'ordenaPecas' ordena o mapa pelo
  y de uma forma crescente.

== Exemplos de utilização:

>>> ordenaPecas [(Porta,(0,2)),(Bloco,(0,3)),(Caixa,(1,2)),(Bloco,(1,3)),(Bloco,(2,3))]
[(Porta,(0,2)),(Caixa,(1,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]

== Código:
@
ordenaPecas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaPecas [] = []
ordenaPecas (h:t) = inserePeca h (ordenaPecas t)
    where inserePeca p [] = [p]
          inserePeca (p1,(x1,y1)) ((p2,(x2,y2)):z)
               |y1 > y2 || y1 == y2 && x1 > x2  = (p2,(x2,y2)):inserePeca (p1,(x1,y1)) z
               |y1 < y2 || y1 == y2 && x1 <= x2 = (p1,(x1,y1)):(p2,(x2,y2)):z
@
-}
ordenaPecas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordenaPecas [] = []
ordenaPecas (h:t) = inserePeca h (ordenaPecas t)
    where inserePeca p [] = [p]
          inserePeca (p1,(x1,y1)) ((p2,(x2,y2)):z)
               |y1 > y2 || y1 == y2 && x1 > x2  = (p2,(x2,y2)):inserePeca (p1,(x1,y1)) z
               |y1 < y2 || y1 == y2 && x1 <= x2 = (p1,(x1,y1)):(p2,(x2,y2)):z

{- | A função 'chaoContinuo' verifica se o chao do mapa
  é contínuo, ou seja, verifica se não existe nenhuma falha 
  no chao do mapa.
  
  Vai começar no ultimo bloco da primeira coluna (no bloco em x=0 com o maior y) e a partir desse bloco
  vai verificar se existe um bloco a sua direita, por cima, embaixo 
  ou na diagonal,se verificar que existe um bloco em alguma dessas direções a função elemina o bloco em que
  estavamos e testa para o seguinte bloco.Ao chegar ao ultimo bloco a função da output a 
  True. Se houver algum bloco que falha ao longo do caminha a função verifica que esse bloco não
  existe e dá output a False.
  
  Esta função utiliza as funções 'ordenaPecas' e 'removeCertaPeca' 
  

== Exemplos de utilização:

>>> chaoContinuo 2 (Bloco,(0,3)) [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
True
>>> chaoContinuo 2 (Bloco,(0,3)) [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (2,3))]
False

== Código:
@
chaoContinuo :: Int -> (Peca,Coordenadas) -> [(Peca,Coordenada)] -> Bool
chaoContinuo k (Bloco,(x1,y1)) [] = x1 == k 
chaoContinuo k (Bloco,(x1,y1)) l
  |x1 == k = True
  |elem (Bloco,(x1+1,y1)) w = chaoContinuo k (Bloco,(x1+1,y1)) ('removeCertaPeca' (Bloco,(x1+1,y1)) w)
  |elem (Bloco,(x1,y1+1)) w = chaoContinuo k (Bloco,(x1,y1+1)) ('removeCertaPeca' (Bloco,(x1,y1+1)) w)
  |elem (Bloco,(x1,y1-1)) w = chaoContinuo k (Bloco,(x1,y1-1)) ('removeCertaPeca' (Bloco,(x1,y1-1)) w)
  |elem (Bloco,(x1+1,y1+1)) w = chaoContinuo k (Bloco,(x1+1,y1+1)) ('removeCertaPeca' (Bloco,(x1+1,y1+1)) w)
  |elem (Bloco,(x1+1,y1-1)) w = chaoContinuo k (Bloco,(x1+1,y1-1)) ('removeCertaPeca' (Bloco,(x1+1,y1-1)) w)
  |otherwise = False
  where w = 'ordenaPecas' l
@
-}
chaoContinuo :: Int                       -- ^Este valor é o maior x do mapa que recebemos na função 'validaPotencialMapa'
                -> (Peca,Coordenadas)     -- ^Este é o par variavel que começa com a peça com o maior y quando x = 0 da lista que recebemos na função 'validaPotencialMapa'
                -> [(Peca, Coordenadas)]  -- ^Estas lista é o mapa que recebemos na função 'validaPotencialMapa'
                -> Bool                   -- ^Resultado
chaoContinuo k (Bloco,(x1,y1)) [] = x1 == k
chaoContinuo k (Bloco,(x1,y1)) l
  |x1 == k = True
  |(Bloco,(x1+1,y1)) `elem` w = chaoContinuo k (Bloco,(x1+1,y1)) (removeCertaPeca (Bloco,(x1+1,y1)) w)
  |(Bloco,(x1,y1+1)) `elem` w = chaoContinuo k (Bloco,(x1,y1+1)) (removeCertaPeca (Bloco,(x1,y1+1)) w)
  |(Bloco,(x1,y1-1)) `elem` w = chaoContinuo k (Bloco,(x1,y1-1)) (removeCertaPeca (Bloco,(x1,y1-1)) w)
  |(Bloco,(x1+1,y1+1)) `elem` w = chaoContinuo k (Bloco,(x1+1,y1+1)) (removeCertaPeca (Bloco,(x1+1,y1+1)) w)
  |(Bloco,(x1+1,y1-1)) `elem` w = chaoContinuo k (Bloco,(x1+1,y1-1)) (removeCertaPeca (Bloco,(x1+1,y1-1)) w)
  |otherwise = False
  where w = ordenaPecas l



{- | A função 'removeCertaPeca' remove uma peça dada a um mapa dado se existir nele.

== Exemplos de utilização:
>>> removeCertaPeca (Porta,(0,2)) [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
[(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]

== Código:
@
removeCertaPeca :: (Peca,Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removeCertaPeca (_,_) [] = []
removeCertaPeca p1 (p2:ps) 
 |p1 == p2 = ps
 |otherwise = p2:removeCertaPeca p1 ps
@
-}
removeCertaPeca :: (Peca,Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removeCertaPeca (_,_) [] = []
removeCertaPeca p1 (p2:ps)
 |p1 == p2 = ps
 |otherwise = p2:removeCertaPeca p1 ps

{- | A função 'xMax' da output ao maior x de um dado mapa.

== Exemplos de utilização:

>>> xMax [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
2

== Código:
@
xMax :: [(Peca, Coordenadas)] -> Int
xMax [] = 0
xMax [(p1,(x1,y1))] = x1
xMax ((p1,(x1,y1)):(p2,(x2,y2)):t)
 |x1 >= x2 = xMax ((p1,(x1,y1)):t)
 |otherwise = xMax ((p2,(x2,y2)):t)
@
-}
xMax :: [(Peca, Coordenadas)] -> Int
xMax [] = 0
xMax [(p1,(x1,y1))] = x1
xMax ((p1,(x1,y1)):(p2,(x2,y2)):t)
 |x1 >= x2 = xMax ((p1,(x1,y1)):t)
 |otherwise = xMax ((p2,(x2,y2)):t)

{- | A função 'colunaUmBlocos' da output aos blocos da primeira coluna de um certo mapa ordenado.

Esta função utiliza a função 'ordenaPecas'.

== Exemplos de utilização:

>>> colunaUmBlocos [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
[(Bloco,(0,3))]

== Código:
@
colunaUmBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
colunaUmBlocos [] = []
colunaUmBlocos l
 |(x1 == 0) && (p1 == Bloco) = (p1,(x1,y1)):colunaUmBlocos t
 |otherwise = colunaUmBlocos t
 where ((p1,(x1,y1)):t) = ordenaPecas l
@
-}
colunaUmBlocos :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
colunaUmBlocos [] = []
colunaUmBlocos l
 |x1 == 0 && p1 == Bloco = (p1,(x1,y1)):colunaUmBlocos t
 |otherwise = colunaUmBlocos t
 where ((p1,(x1,y1)):t) = ordenaPecas l

{- | A função 'maiorY' da output a peça com o maior y da primeira coluna de um certo mapa.

Esta função utiliza a função 'colunaUmBlocos'.

== Exemplos de utilização:

>>> maiorY [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
(Bloco,(0,3))

== Código:
@
maiorY :: [(Peca, Coordenadas)] ->  (Peca, Coordenadas)
maiorY [h] = h
maiorY l = last t
 where t = colunaUmBlocos l
@
-}
maiorY :: [(Peca, Coordenadas)]     -- ^Nunca recebe lista vazia 
          ->  (Peca, Coordenadas)   -- ^Resultado
maiorY [h] = h
maiorY l = last t
 where t = colunaUmBlocos l