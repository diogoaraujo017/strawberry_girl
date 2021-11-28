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

{- | A função 'constroiMapa' recebe uma lista de peças e coordenadas e transforma
  essa lista numa lista de listas de peças que cada lista corresponde a uma linha do
  mapa.

  Esta função utiliza as funções 'mapaFinal', 'colocaPeca' e 'removePecasIguais'.

== Exemplos de utilização:
>>> constroiMapa [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,3))]
[[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]

== Código:
@
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = 'mapaFinal' z ('colocaPeca' (0,0) z z)
  where z = 'removePecasIguais' l
@ 
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l = mapaFinal z (colocaPeca (0,0) z z)
  where z = removePecasIguais l

{- | A função 'removePecasIguais' recebe uma lista e verifica se tem peças iguais,
  se tiver remove certas peças até que não hajam peças iguais.

== Exemplos de utilização:

>>> removePecasIguais [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Porta,(0,2)),(Bloco, (2,4))]
[(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,4))]
>>> removePecasIguais [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,4))]
[(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,4))]

== Código:
@
removePecasIguais :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removePecasIguais [] = []
removePecasIguais [(p,(x,y))] = [(p,(x,y))]
removePecasIguais l
 |p1 == p1 && x1==x2 && y1==y2 = removePecasIguais ((p1,(x1,y1)):t)
 |otherwise = (p1,(x1,y1)):removePecasIguais ((p2,(x2,y2)):t)
 where (p1,(x1,y1)):(p2,(x2,y2)):t = 'ordenaPecas' l
@ 
-}
removePecasIguais :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removePecasIguais [] = []
removePecasIguais [(p,(x,y))] = [(p,(x,y))]
removePecasIguais l
 |p1 == p1 && x1==x2 && y1==y2 = removePecasIguais ((p1,(x1,y1)):t)
 |otherwise = (p1,(x1,y1)):removePecasIguais ((p2,(x2,y2)):t)
 where (p1,(x1,y1)):(p2,(x2,y2)):t = ordenaPecas l

{- | A função 'colocaPeca' recebe uma lista de peças e as suas coordenadas
 e tranforma essa lista numa lista de peças em ordem com 
 os respetivos vazios inseridos nos seus respetivos lugares.

== Exemplos de utilização:
>>> colocaPecas (0,0) [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,3))] [(Porta, (0,2)),(Bloco, (0,3)),(Bloco, (1,3)),(Bloco, (2,3))]
[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Bloco,Bloco]

== Código:
@
colocaPeca :: (Int,Int) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [Peca]
colocaPeca (x,y) [] l
 |x > m = []
 |otherwise = Vazio:colocaPeca (x+1,y) [] l
 where m = 'xMax' l
colocaPeca (x,y) l l1
 |x==x1 && y==y1 = p1:colocaPeca (x+1,y) t l1
 |x/=x1 && y==y1 = Vazio:colocaPeca (x+1,y) ((p1,(x1,y1)):t) l1
 |y/=y1 && x<xm  = Vazio:colocaPeca (x+1,y) ((p1,(x1,y1)):t) l1
 |y/=y1 && x==xm = Vazio:colocaPeca (0,y+1) ((p1,(x1,y1)):t) l1
 |y/=y1 && x>xm = colocaPeca (0,y+1) ((p1,(x1,y1)):t) l1
 where (p1,(x1,y1)):t = 'ordenaPecas' l
       xm = 'xMax' l1
@
-}
colocaPeca :: (Int,Int)                  -- ^Acumulador que dita as coordenadas em que nos encontramos.
              -> [(Peca, Coordenadas)]   -- ^Lista inicial de peças e coordenadas que vamos circular.
              -> [(Peca, Coordenadas)]   -- ^Lista inicial que não muda, que apenas serve para obter o 'xMax' da lista inicial.
              -> [Peca]                  -- ^Resultado
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

{- | A função 'mapaFinal' tranforma uma lista de peças por ordem com os respetivos vazios, numa lista de listas (Mapa),
  onde as listas são as linhas do mapa.

  A função é capaz de realizar isto devido à utilização das funções 'takePecas','removePecas' e 'xMax'.

  Através da função 'takePecas' e 'xMax' conseguimos fazer uma lista com a primeira linha do mapa.

  Através da função 'removePecas' e 'xMax' conseguimos fazer a recursividade da função 'mapaFinal' para a lista de peças sem a primeira linha.

== Exemplos de utilização:

>>> mapafinal [(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))] [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Bloco,Bloco]
[[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]

== Código:
@
mapaFinal :: [(Peca, Coordenadas)] -> [Peca] -> Mapa
mapaFinal l [] = []
mapaFinal l m = 'takePecas' ('xMax' l+1) m:mapaFinal l ('removePecas' ('xMax' l+1) m)
@ 
-}
mapaFinal :: [(Peca, Coordenadas)]  -- ^Lista inicial que recebe na função 'constroiMapa' que não sofre alterações pois apenas serve para obter o 'xMax' da lista. 
             -> [Peca]              -- ^Lista de peças que recebe da função 'colocaPeca' quando chamamos a função 'constroiMapa'.
             -> Mapa                -- ^Resultado
mapaFinal l [] = []
mapaFinal l m = takePecas (xMax l+1) m:mapaFinal l (removePecas (xMax l+1) m)

{- | A função 'takePecas' faz uma lista peças com as primeiras x peças de uma lista de peças. 

== Exemplos de utilização:

>>> takePecas 3 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Bloco,Bloco]
[Vazio,Vazio,Vazio]

== Código:
@
takePecas :: Int -> [Peca] -> [Peca]
takePecas _ [] = []
takePecas 0 l  = []
takePecas x (h:t) = h:takePecas (x-1) t
@ 
-}
takePecas :: Int -> [Peca] -> [Peca]
takePecas _ [] = []
takePecas 0 l  = []
takePecas x (h:t) = h:takePecas (x-1) t

{- | A função 'removePecas' retira as primeiras x peças de uma lista de peças. 

== Exemplos de utilização:

>>> removePecas 3 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Bloco,Bloco]
[Vazio,Vazio,Vazio,Porta,Vazio,Vazio,Bloco,Bloco,Bloco]

== Código:
@
removePecas :: Int -> [Peca] -> [Peca]
removePecas _ [] = []
removePecas 0 m = m
removePecas x (h:t) = removePecas (x-1) t
@ 
-}
removePecas :: Int -> [Peca] -> [Peca]
removePecas _ [] = []
removePecas 0 m = m
removePecas x (h:t) = removePecas (x-1) t

{- | A função 'desconstroiMapa' pega num Mapa e tranforma esse mapa na lista das coordenadas ocultando os 
vazios. Esta função utiliza a função auxiliar 'somaPos'.

== Exemplos de utilização:

>>> desconstroiMapa [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
[(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]

== Código:
@
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa m = 'somaPos' (0,0) m
@ 
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa m = somaPos (0,0) m

{- | A função 'somaPos' tranforma um mapa numa lista de pares peça coordenada com a utilização de um
  acumulador que a medida que se circula o mapa vai mantendo as coordenadas em que nos encontramos
  tornando assim possivel atribuir coordenadas as peças.

   Esta função não dá output aos vazios e as suas devidas coordenadas.

== Exemplos de utilização:

>>> somaPos (0,0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
[(Porta,(0,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]

== Código:
@
somaPos :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)]
somaPos (_,_) [] = []
somaPos (x,y) ([]:t) = somaPos (0,y+1) t
somaPos (x,y) ((p:ps):t) = case p of Vazio -> somaPos (x+1,y) (ps:t)
                                     _ -> (p,(x,y)):somaPos (x+1,y) (ps:t)
@ 
-}
somaPos :: (Int,Int)                 -- ^Acumulador que relata as coordenadas das peças.
           -> Mapa                   -- ^Mapa inicial que é circulado
           -> [(Peca, Coordenadas)]  -- ^Resultado
somaPos (_,_) [] = []
somaPos (x,y) ([]:t) = somaPos (0,y+1) t
somaPos (x,y) ((p:ps):t) = case p of Vazio -> somaPos (x+1,y) (ps:t)
                                     _ -> (p,(x,y)):somaPos (x+1,y) (ps:t)


{- | A função 'desconstroiMapa2' pega num Mapa e tranforma esse mapa na lista das coordenadas mostrando também os 
vazios e as suas devidas coordenadas. Esta função utiliza a função auxiliar 'somaPos2'.

== Exemplos de utilização:

>>> desconstroiMapa2 [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
[(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]

== Código:
@
desconstroiMapa2 :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa2 [] = []
desconstroiMapa2 m = 'somaPos2' (0,0) m
@ 
-}
desconstroiMapa2 :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa2 [] = []
desconstroiMapa2 m = somaPos2 (0,0) m


{- | A função 'somaPos2' tranforma um mapa numa lista de pares peça coordenada com a utilização de um
  acumulador que à medida que se circula o mapa vai mantendo as coordenadas em que nos encontramos
  tornando assim possivel atribuir coordenadas ás peças.

  Esta função também dá output aos vazios e as suas devidas coordenadas.

== Exemplos de utilização:

>>> somaPos2 (0,0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
[(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]

== Código:
@
somaPos2 :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)]
somaPos2 (_,_) [] = []
somaPos2 (x,y) ([]:t) = somaPos2 (0,y+1) t
somaPos2 (x,y) ((p:ps):t) = (p,(x,y)):somaPos2 (x+1,y) (ps:t)
@ 
-}
somaPos2 :: (Int,Int) -> Mapa -> [(Peca, Coordenadas)]
somaPos2 (_,_) [] = []
somaPos2 (x,y) ([]:t) = somaPos2 (0,y+1) t
somaPos2 (x,y) ((p:ps):t) = (p,(x,y)):somaPos2 (x+1,y) (ps:t)