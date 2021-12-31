{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa4_2021li1g097
Description : Movimentação do personagem
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g097 where


import LI12122
import Tarefa3_2021li1g097 () 
import Tarefa2_2021li1g097 ( constroiMapa, desconstroiMapa )
import Tarefa1_2021li1g097 ( removeCertaPeca )

{- | A função 'moveJogador' vai realizar um movimento que altera a posição 
do jogador e pode também alterar o layout do mapa que esta a ser jogado.
Esta função começa por verificar se pode executar o movimento que lhe
esta a ser pedido, aplicando-lhe depois o movimento. 

Esta função utiliza funções que se encontram posteriormente defenidas.

== Código:
@
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo (p:ps) (Jogador (x,y) d b)) m
 |m == AndarEsquerda = if not b
                       then if 'verificaLados' (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                            then if 'vazioEmbaixo' (0,0) (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                                 then 'jogadorCai' (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                                 else Jogo (p:ps) (Jogador (x-1,y) Oeste b)
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)
                       else if 'verificaLadosCaixa' (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                            then if 'vazioEmbaixo' (0,0) (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                                 then 'colocaCaixaCima' (0,0) removeCaixa  ('jogadorCai' (Jogo (p:ps) (Jogador (x-1,y) Oeste b)))
                                 else 'colocaCaixaCima' (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)

 |m == AndarDireita = if not b
                      then if 'verificaLados' (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                           then if 'vazioEmbaixo' (0,0) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                then 'jogadorCai' (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                else Jogo (p:ps) (Jogador (x+1,y) Este b)
                           else Jogo (p:ps) (Jogador (x,y) Este b)
                      else if 'verificaLadosCaixa' (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                           then if 'vazioEmbaixo' (0,0) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                then 'colocaCaixaCima' (0,0) removeCaixa  ('jogadorCai' (Jogo (p:ps) (Jogador (x+1,y) Este b)))
                                else 'colocaCaixaCima' (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x+1,y) Este b))
                           else Jogo (p:ps) (Jogador (x,y) Este b)

 |m == Trepar = if not b
                then if 'podeTrepar' (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then if d == Oeste
                          then Jogo (p:ps) (Jogador (x-1,y-1) Oeste b)
                          else Jogo (p:ps) (Jogador (x+1,y-1) Este b)
                     else Jogo (p:ps) (Jogador (x,y) d b)
                else if 'podeTreparCaixa' (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then if d == Oeste
                          then 'colocaCaixaCima' (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x-1,y-1) Oeste b))
                          else 'colocaCaixaCima' (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x+1,y-1) Este b))
                     else Jogo (p:ps) (Jogador (x,y) d b)

 |m == InterageCaixa = if not b
                       then if 'podePegar' (0,0) (Jogo (p:ps) (Jogador (x,y) d b))     
                            then if d == Oeste     
                                 then 'colocaCaixaCima' (0,0) ('removeCertaPeca' (Caixa,(x-1,y)) ('desconstroiMapa' (p:ps))) (Jogo (p:ps) (Jogador (x,y) Oeste b))
                                 else 'colocaCaixaCima' (0,0) ('removeCertaPeca' (Caixa,(x+1,y)) ('desconstroiMapa' (p:ps))) (Jogo (p:ps) (Jogador (x,y) Este b))              
                            else Jogo (p:ps) (Jogador (x,y) d b)   
                       else if 'podeLargar' (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) 
                            then if 'vaiAtirar' (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                 then if d == Oeste
                                      then Jogo ('constroiMapa' ('removeCaixa'  ++ [(Caixa,(x-1,'pecaCoordenada' y ('certaColuna' (x-1) (0,0) (p:ps))))]))  (Jogador (x,y) Oeste b) 
                                      else Jogo ('constroiMapa' ('removeCaixa'  ++ [(Caixa,(x+1,'pecaCoordenada' y ('certaColuna' (x+1) (0,0) (p:ps))))]))  (Jogador (x,y) Este b)
                                 else if vazioFrente (0,0) (Jogo (p:ps) (Jogador (x,y) d b))     
                                      then if d == Oeste 
                                           then Jogo ('constroiMapa' ('removeCaixa'  ++ [(Caixa,(x-1,y))]))  (Jogador (x,y) Oeste b)
                                           else Jogo ('constroiMapa' ('removeCaixa'  ++ [(Caixa,(x+1,y))]))  (Jogador (x,y) Este b) 
                                      else if d == Oeste 
                                           then Jogo ('constroiMapa' ('removeCaixa'  ++ [(Caixa,(x-1,y-1))])) (Jogador (x,y) Oeste b)      
                                           else Jogo ('constroiMapa' ('removeCaixa'  ++ [(Caixa,(x+1,y-1))])) (Jogador (x,y) Este b) 
                            else Jogo (p:ps) (Jogador (x,y) d b) 
 where removeCaixa = 'removeCertaPeca' (Caixa,(x,y-1)) ('desconstroiMapa' (p:ps))
@
-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo (p:ps) (Jogador (x,y) d b)) m
 |m == AndarEsquerda = if not b
                       then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                            then if vazioEmbaixo (0,0) (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                                 then jogadorCai (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                                 else Jogo (p:ps) (Jogador (x-1,y) Oeste b)
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)
                       else if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarEsquerda
                            then if vazioEmbaixo (0,0) (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                                 then colocaCaixaCima (0,0) removeCaixa  (jogadorCai (Jogo (p:ps) (Jogador (x-1,y) Oeste b)))
                                 else colocaCaixaCima (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)

 |m == AndarDireita = if not b
                      then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                           then if vazioEmbaixo (0,0) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                then jogadorCai (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                else Jogo (p:ps) (Jogador (x+1,y) Este b)
                           else Jogo (p:ps) (Jogador (x,y) Este b)
                      else if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                           then if vazioEmbaixo (0,0) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                then colocaCaixaCima (0,0) removeCaixa  (jogadorCai (Jogo (p:ps) (Jogador (x+1,y) Este b)))
                                else colocaCaixaCima (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x+1,y) Este b))
                           else Jogo (p:ps) (Jogador (x,y) Este b)

 |m == Trepar = if not b
                then if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then if d == Oeste
                          then Jogo (p:ps) (Jogador (x-1,y-1) Oeste b)
                          else Jogo (p:ps) (Jogador (x+1,y-1) Este b)
                     else Jogo (p:ps) (Jogador (x,y) d b)
                else if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then if d == Oeste
                          then colocaCaixaCima (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x-1,y-1) Oeste b))
                          else colocaCaixaCima (0,0) removeCaixa  (Jogo (p:ps) (Jogador (x+1,y-1) Este b))
                     else Jogo (p:ps) (Jogador (x,y) d b)

 |m == InterageCaixa = if not b
                       then if podePegar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))     
                            then if d == Oeste     
                                 then colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x-1,y)) (desconstroiMapa (p:ps))) (Jogo (p:ps) (Jogador (x,y) Oeste b))
                                 else colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x+1,y)) (desconstroiMapa (p:ps))) (Jogo (p:ps) (Jogador (x,y) Este b))              
                            else Jogo (p:ps) (Jogador (x,y) d b)   
                       else if podeLargar (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) 
                            then if vaiAtirar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                 then if d == Oeste
                                      then Jogo (constroiMapa (removeCaixa  ++ [(Caixa,(x-1,pecaCoordenada y (certaColuna (x-1) (0,0) (p:ps))))]))  (Jogador (x,y) Oeste False) 
                                      else Jogo (constroiMapa (removeCaixa  ++ [(Caixa,(x+1,pecaCoordenada y (certaColuna (x+1) (0,0) (p:ps))))]))  (Jogador (x,y) Este False )
                                 else if vazioFrente (0,0) (Jogo (p:ps) (Jogador (x,y) d b))     
                                      then if d == Oeste 
                                           then Jogo (constroiMapa (removeCaixa  ++ [(Caixa,(x-1,y))]))  (Jogador (x,y) Oeste False)
                                           else Jogo (constroiMapa (removeCaixa  ++ [(Caixa,(x+1,y))]))  (Jogador (x,y) Este False) 
                                      else if d == Oeste 
                                           then Jogo (constroiMapa (removeCaixa  ++ [(Caixa,(x-1,y-1))])) (Jogador (x,y) Oeste False)      
                                           else Jogo (constroiMapa (removeCaixa  ++ [(Caixa,(x+1,y-1))])) (Jogador (x,y) Este False) 
                            else Jogo (p:ps) (Jogador (x,y) d True) 
 where removeCaixa = removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))

{- | A função 'vazioEmbaixo' vai verificar se existe um vazio embaixo do jogador

== Código:
@
vazioEmbaixo :: (Int,Int) -> Jogo -> Bool
vazioEmbaixo (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioEmbaixo (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioEmbaixo (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2+1 = x == Vazio
 |otherwise = vazioEmbaixo (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b))
@
-}
vazioEmbaixo :: (Int,Int)    -- ^Acumulador que nos permite manter a informação da coordenada em que vamos
                -> Jogo      -- ^Jogo
                -> Bool      -- ^Resultado
vazioEmbaixo (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioEmbaixo (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioEmbaixo (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2+1 = x == Vazio || x == Porta
 |otherwise = vazioEmbaixo (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b))

{- | A função 'vazioEmcima' vai verificar se existe um vazio por cima do jogador.

== Código:
@
vazioEmcima :: (Int,Int) -> Jogo -> Bool   
vazioEmcima (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioEmcima (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioEmcima (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2-1 = x == Vazio
 |otherwise = vazioEmcima (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b))
@
-}
vazioEmcima :: (Int,Int)    -- ^Acumulador que nos permite manter a informação da coordenada em que vamos
                -> Jogo      -- ^Jogo
                -> Bool      -- ^Resultado
vazioEmcima (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioEmcima (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioEmcima (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2-1 = x == Vazio
 |otherwise = vazioEmcima (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b))

{- | A função 'verificaLados' vai verificar se o Jogador pode andar para a direita ou para a esquerda.

Se o jogador quiser andar para a direita a função vai verificar, se 
quando o jogador esta sem caixa, existe um vazio a sua frente para onde se
pode deslocar. A função faz as mesmas verificações para quando o jogador 
quer andar para a esquerda.

== Código:
@
verificaLados :: (Int,Int) -> Jogo -> Movimento -> Bool         
verificaLados (x1,y1) (Jogo [] (Jogador (x2,y2) d b)) m = False
verificaLados (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) m = verificaLados (0,y1+1) (Jogo t (Jogador (x2,y2) d b)) m
verificaLados (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarEsquerda
  |y1 == y2 && x1 == x2-1 = x == Vazio || x == Porta
  |otherwise = verificaLados (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarEsquerda
verificaLados (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarDireita
  |y1 == y2 && x1 == x2+1 = x == Vazio || x == Porta
  |otherwise = verificaLados (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarDireita
@
-}
verificaLados :: (Int,Int)       -- ^Acumulador que nos permite verificar a peça a frente do jogador
                 -> Jogo         -- ^Jogo
                 -> Movimento    -- ^Movimento que é dado         
                 -> Bool         -- ^Resultado
verificaLados (x1,y1) (Jogo [] (Jogador (x2,y2) d b)) m = False
verificaLados (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) m = verificaLados (0,y1+1) (Jogo t (Jogador (x2,y2) d b)) m
verificaLados (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarEsquerda
  |y1 == y2 && x1 == x2-1 = x == Vazio || x == Porta
  |otherwise = verificaLados (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarEsquerda
verificaLados (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarDireita
  |y1 == y2 && x1 == x2+1 = x == Vazio || x == Porta
  |otherwise = verificaLados (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarDireita

{- | A função 'verificaLadosCaixa' vai verificar se o jogador pode andar para
a esquerda ou para a direita carregando uma caixa.

-Se o jogador quiser andar para a direita a função vai verificar, se 
quando o jogador esta com caixa,  existe um vazio a frente do jogador e um vazio a frente da caixa.
A função faz as mesmas verificações para quando o jogador quer andar para a esquerda.

== Código:
@
verificaLadosCaixa :: (Int,Int) -> Jogo -> Movimento -> Bool 
verificaLadosCaixa (_,y1) (Jogo [] (Jogador (x2,y2) d b)) m = False
verificaLadosCaixa (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) m = verificaLadosCaixa (0,y1+1) (Jogo t (Jogador (x2,y2) d b)) m
verificaLadosCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarEsquerda
 |y1 == y2-1 && x1 == x2-1 = x == Vazio && verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarEsquerda
 |y1 == y2 && x1 == x2-1 = x == Vazio || x == Porta
 |otherwise = verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarEsquerda
verificaLadosCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarDireita
 |y1 == y2-1  && x1 == x2+1 = x == Vazio && verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarDireita
 |y1 == y2 && x1 == x2+1 = x == Vazio || x == Porta
 |otherwise = verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarDireita
@
-}
verificaLadosCaixa :: (Int,Int)          -- ^Acumulador de coordenadas que nos permite verificar a peça a frente do jogador
                      -> Jogo            -- ^Jogo
                      -> Movimento       -- ^Movimento que é recebido 
                      -> Bool            -- ^Resultado
verificaLadosCaixa (_,y1) (Jogo [] (Jogador (x2,y2) d b)) m = False
verificaLadosCaixa (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) m = verificaLadosCaixa (0,y1+1) (Jogo t (Jogador (x2,y2) d b)) m
verificaLadosCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarEsquerda
 |y1 == y2-1 && x1 == x2-1 = x == Vazio && verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarEsquerda
 |y1 == y2 && x1 == x2-1 = x == Vazio || x == Porta
 |otherwise = verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarEsquerda
verificaLadosCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b)) AndarDireita
 |y1 == y2-1  && x1 == x2+1 = x == Vazio && verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarDireita
 |y1 == y2 && x1 == x2+1 = x == Vazio || x == Porta
 |otherwise = verificaLadosCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b)) AndarDireita

{- | A função 'podeTrepar' vai verificar se o jogador pode realizar o movimento trepar
sem caixa, para o lado em que esta virado.

Se o jogador quiser trepar sem caixa a função vai verificar se para o lado que o jogador
 esta virado existe um Bloco ou uma Caixa a sua frente, se existe um Vazio encima desse 
 Bloco ou Caixa e ainda se existe um vazio encima do jogador, so assim o jogador pode trepar.

== Código:
@
podeTrepar :: (Int,Int) -> Jogo -> Bool      
podeTrepar _ (Jogo [] Jogador {}) = False
podeTrepar (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b))  = podeTrepar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podeTrepar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-1 = x == Vazio && a
 |x1 == x2 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Bloco || x == Caixa
 |otherwise = a
 where a = podeTrepar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podeTrepar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2 = x == Caixa || x == Bloco
 |otherwise = b
 where b = podeTrepar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))
@
-}
podeTrepar :: (Int,Int)       -- ^Acumulador de coordenadas que nos permite verificar as peças nas coordenadas na diagonal e a frente do jogador
              -> Jogo         -- ^Jogo
              -> Bool         -- ^Resultado
podeTrepar _ (Jogo [] Jogador {}) = False
podeTrepar (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b))  = podeTrepar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podeTrepar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-1 = (x == Vazio || x == Porta) && a
 |x1 == x2 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Bloco || x == Caixa
 |otherwise = a
 where a = podeTrepar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podeTrepar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-1 = (x == Vazio || x == Porta) && b
 |x1 == x2+1 && y1 == y2 = x == Caixa || x == Bloco
 |otherwise = b
 where b = podeTrepar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

{- | A função 'podeTreparCaixa' vai verificar se o jogador pode realizar o movimento trepar
com caixa, para o lado em que esta virado.

 A função vai verificar se para o lado que o jogador esta virado existe um Bloco ou uma Caixa a sua frente, se existe um Vazio encima da caixa
 que o jogador esta a carregar, se existe um Vazio encima do Bloco ou Caixa que quer trepar e 
 ainda se existe um Vazio onde a caixa que o jogador leva encima se vai encontrar quando o 
 jogador trepar o Bloco ou Caixa.

== Código:
@
podeTreparCaixa :: (Int,Int) -> Jogo -> Bool  
podeTreparCaixa _ (Jogo [] Jogador {}) = False
podeTreparCaixa (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = podeTreparCaixa (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podeTreparCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-2 = x == Vazio && a
 |x1 == x2 && y1 == y2-2 = x == Vazio && a
 |x1 == x2-1 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Bloco || x == Caixa
 |otherwise = a
 where a = podeTreparCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podeTreparCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2 && y1 == y2-2 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-2 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2 = x == Bloco || x == Caixa
 |otherwise = b
 where b = podeTreparCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))
@
-}
podeTreparCaixa :: (Int,Int)          -- ^Acumulador de coordenadas que nos permite verificar as peças
                   -> Jogo            -- ^Jogo
                   -> Bool            -- ^Resultado
podeTreparCaixa _ (Jogo [] Jogador {}) = False
podeTreparCaixa (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = podeTreparCaixa (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podeTreparCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-2 = x == Vazio && a
 |x1 == x2 && y1 == y2-2 = x == Vazio && a
 |x1 == x2-1 && y1 == y2-1 = (x == Vazio || x == Porta) && a
 |x1 == x2-1 && y1 == y2 = x == Bloco || x == Caixa
 |otherwise = a
 where a = podeTreparCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podeTreparCaixa (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2 && y1 == y2-2 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-2 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-1 = (x == Vazio || x == Porta) && b
 |x1 == x2+1 && y1 == y2 = x == Bloco || x == Caixa
 |otherwise = b
 where b = podeTreparCaixa (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

{- | A função 'jogadorCai' vai verificar se o jogador vai cai num precipício e vai também
executar o movimento de cair parando o jogador de cair quando tiver um obstaculo em baixo dele mesmo.

== Código:
@
jogadorCai :: Jogo -> Jogo
jogadorCai (Jogo m (Jogador (x,y) d b))
 |vazioEmbaixo (0,0) (Jogo m (Jogador (x,y) d b)) = jogadorCai (Jogo m (Jogador (x,y+1) d b))
 |otherwise = Jogo m (Jogador (x,y) d b)
@
-}
jogadorCai :: Jogo -> Jogo
jogadorCai (Jogo m (Jogador (x,y) d b))
 |vazioEmbaixo (0,0) (Jogo m (Jogador (x,y) d b)) = jogadorCai (Jogo m (Jogador (x,y+1) d b))
 |otherwise = Jogo m (Jogador (x,y) d b)

{- | A função 'colocaCaixaCima' vai colocar uma caixa encima do jogador.

== Código:
@
colocaCaixaCima :: (Int,Int) -> [(Peca,Coordenadas)] -> Jogo -> Jogo
colocaCaixaCima (x1,y1) l (Jogo ([]:t) (Jogador (x2,y2) d b)) = colocaCaixaCima (0,y1+1) l (Jogo t (Jogador (x2,y2) d b))
colocaCaixaCima (x1,y1) l (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2-1 = Jogo (constroiMapa (l ++ [(Caixa,(x1,y1))])) (Jogador (x2,y2) d True)
 |otherwise = colocaCaixaCima (x1+1,y1) l (Jogo (xs:t) (Jogador (x2,y2) d b))
@
-}
colocaCaixaCima :: (Int,Int) -> [(Peca,Coordenadas)] -> Jogo -> Jogo
colocaCaixaCima (x1,y1) l (Jogo ([]:t) (Jogador (x2,y2) d b)) = colocaCaixaCima (0,y1+1) l (Jogo t (Jogador (x2,y2) d b))
colocaCaixaCima (x1,y1) l (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2-1 = Jogo (constroiMapa (l ++ [(Caixa,(x1,y1))])) (Jogador (x2,y2) d True)
 |otherwise = colocaCaixaCima (x1+1,y1) l (Jogo (xs:t) (Jogador (x2,y2) d b))

{- | A função 'podePegar' vai verificar se o jogador pode pegar numa caixa.

Se o jogador quiser pegar numa caixa a função vai verificar se o jogador não tem uma caixa encima,
 se existe um vazio encima do jogador, se existe uma caixa para pegar a frente do jogador
 e ainda se existe um vazio encima dessa caixa, so assim ele poderá pegar numa caixa.

== Código:
@
podePegar :: (Int,Int) -> Jogo -> Bool
podePegar _ (Jogo [] Jogador {}) = False
podePegar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = podePegar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podePegar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-1 = x == Vazio && a
 |x1 == x2 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Caixa
 |otherwise = a
 where a = podePegar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podePegar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2 = x == Caixa
 |otherwise = b
 where b = podePegar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))
@
-}
podePegar :: (Int,Int) -> Jogo -> Bool
podePegar _ (Jogo [] Jogador {}) = False
podePegar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = podePegar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podePegar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-1 = x == Vazio && a
 |x1 == x2 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Caixa
 |otherwise = a
 where a = podePegar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podePegar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2 = x == Caixa
 |otherwise = b
 where b = podePegar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

{- | A função 'podeLargar' vai verificar se o jogador pode largar uma caixa.

Se o jogador quiser largar uma caixa a função vai verificar se o jogador tem uma caixa encima,
 se existe um vazio a frente do jogador e um vazio a frente da caixa ou se existe um Bloco ou 
 uma Caixa com um vazio encima onde o jogador possa colocar a caixa.

== Código:
@
podeLargar :: (Int,Int) -> Jogo -> Bool
podeLargar _ (Jogo [] Jogador {}) = False
podeLargar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = podeLargar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podeLargar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Caixa || x == Vazio || x == Bloco
 |otherwise = a
 where a = podeLargar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podeLargar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2 = x == Caixa || x == Vazio || x == Bloco
 |otherwise = b
 where b = podeLargar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))
@
-}
podeLargar :: (Int,Int) -> Jogo -> Bool
podeLargar _ (Jogo [] Jogador {}) = False
podeLargar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = podeLargar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
podeLargar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2-1 = x == Vazio && a
 |x1 == x2-1 && y1 == y2 = x == Caixa || x == Vazio || x == Bloco
 |otherwise = a
 where a = podeLargar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
podeLargar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2-1 = x == Vazio && b
 |x1 == x2+1 && y1 == y2 = x == Caixa || x == Vazio || x == Bloco
 |otherwise = b
 where b = podeLargar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

{- | A função 'vazioFrente' vai verificar se existe um vazio a frente do jogador.

== Código:
@
vazioFrente :: (Int,Int) -> Jogo -> Bool
vazioFrente _ (Jogo [] Jogador {}) = False
vazioFrente (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioFrente (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioFrente (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2 = x == Vazio
 |otherwise = vazioFrente (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
vazioFrente (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2 = x == Vazio
 |otherwise = vazioFrente (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
@
-}
vazioFrente :: (Int,Int) -> Jogo -> Bool
vazioFrente _ (Jogo [] Jogador {}) = False
vazioFrente (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioFrente (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioFrente (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2 = x == Vazio
 |otherwise = vazioFrente (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
vazioFrente (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2 = x == Vazio
 |otherwise = vazioFrente (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

{- | A função 'vaiAtirar' vai verificar se o jogador vai atirar uma caixa verificando se existe um
vazio na diagonal embaixo e um vazio a sua frente em relação ao jogador.

== Código:
@
vaiAtirar :: (Int,Int) -> Jogo -> Bool
vaiAtirar _ (Jogo [] Jogador {}) = False
vaiAtirar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vaiAtirar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vaiAtirar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2+1 = x == Vazio
 |otherwise = vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
vaiAtirar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2+1 = x == Vazio
 |otherwise = vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))
@
-}
vaiAtirar :: (Int,Int) -> Jogo -> Bool
vaiAtirar _ (Jogo [] Jogador {}) = False
vaiAtirar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vaiAtirar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vaiAtirar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2 = x == Vazio && vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2+1 = x == Vazio
 |otherwise = vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
vaiAtirar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2 = x == Vazio && vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2+1 = x == Vazio
 |otherwise = vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

{- | A função 'certaColuna' vai armazenar numa lista todas as peças de uma coluna com um certo x

== Código:
@
certaColuna :: Int -> (Int,Int) -> Mapa -> [(Peca,Coordenadas)]
certaColuna _ _ [] = []
certaColuna x1 (x2,y2) ([]:t) = certaColuna x1 (0,y2+1) t
certaColuna x1 (x2,y2) ((x:xs):t)
 |x1 == x2 = (x,(x2,y2)):certaColuna x1 (x2+1,y2) (xs:t)
 |otherwise = certaColuna x1 (x2+1,y2) (xs:t)
@
-}

certaColuna :: Int -> (Int,Int) -> Mapa -> [(Peca,Coordenadas)]
certaColuna _ _ [] = []
certaColuna x1 (x2,y2) ([]:t) = certaColuna x1 (0,y2+1) t
certaColuna x1 (x2,y2) ((x:xs):t)
 |x1 == x2 = (x,(x2,y2)):certaColuna x1 (x2+1,y2) (xs:t)
 |otherwise = certaColuna x1 (x2+1,y2) (xs:t)

{- | A função 'pecaCoordenada' vai a procura do primeiro Bloco ou Caixa ou Porta de uma certa coluna com um y maior que o recebido dando output ao y anterior onde existe um vazio.

== Código:
@
pecaCoordenada :: Int -> [(Peca,Coordenadas)] -> Int
pecaCoordenada y1 ((x,(x2,y2)):xs)
 |y1 == y2-1 && x == Vazio = pecaCoordenada (y1+1) xs
 |y1 == y2-1 && (x == Bloco || x == Caixa || x == Porta) = y1
 |otherwise = pecaCoordenada y1 xs

@
-}
pecaCoordenada :: Int -> [(Peca,Coordenadas)] -> Int
pecaCoordenada y1 ((x,(x2,y2)):xs)
 |y1 == y2-1 && x == Vazio = pecaCoordenada (y1+1) xs
 |y1 == y2-1 && (x == Bloco || x == Caixa || x == Porta) = y1
 |otherwise = pecaCoordenada y1 xs

{- | A função 'correrMovimentos' realiza movimentos de uma lista recorrendo a função anteriormente defenida 'moveJogador'.

== Código:
@
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos = foldl moveJogador
@
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos = foldl moveJogador

--random utilities
--[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
-- [[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],[Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
