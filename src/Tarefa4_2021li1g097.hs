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
                                 then colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))) (jogadorCai (Jogo (p:ps) (Jogador (x-1,y) Oeste b)))
                                 else colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))) (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)

 |m == AndarDireita = if not b
                      then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                           then if vazioEmbaixo (0,0) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                then jogadorCai (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                else Jogo (p:ps) (Jogador (x+1,y) Este b)
                           else Jogo (p:ps) (Jogador (x,y) Este b)
                      else if verificaLadosCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) AndarDireita
                           then if vazioEmbaixo (0,0) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                                then colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))) (jogadorCai (Jogo (p:ps) (Jogador (x+1,y) Este b)))
                                else colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                           else Jogo (p:ps) (Jogador (x,y) Este b)

 |m == Trepar = if not b
                then if podeTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then if d == Oeste
                          then Jogo (p:ps) (Jogador (x-1,y-1) Oeste b)
                          else Jogo (p:ps) (Jogador (x+1,y-1) Este b)
                     else Jogo (p:ps) (Jogador (x,y) d b)
                else if podeTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then if d == Oeste
                          then colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))) (Jogo (p:ps) (Jogador (x-1,y-1) Oeste b))
                          else colocaCaixaCima (0,0) (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps))) (Jogo (p:ps) (Jogador (x+1,y-1) Este b))
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
                                      then Jogo (constroiMapa (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps)) ++ [(Caixa,(x-1,pecaCoordenada y (certaColuna (x-1) (0,0) (p:ps))))]))  (Jogador (x,y) Oeste b) 
                                      else Jogo (constroiMapa (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps)) ++ [(Caixa,(x+1,pecaCoordenada y (certaColuna (x+1) (0,0) (p:ps))))]))  (Jogador (x,y) Este b)
                                 else if vazioFrente (0,0) (Jogo (p:ps) (Jogador (x,y) d b))     
                                      then if d == Oeste 
                                           then Jogo (constroiMapa (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps)) ++ [(Caixa,(x-1,y))]))  (Jogador (x,y) Oeste b)
                                           else Jogo (constroiMapa (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps)) ++ [(Caixa,(x+1,y))]))  (Jogador (x,y) Este b) 
                                      else if d == Oeste 
                                           then Jogo (constroiMapa (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps)) ++ [(Caixa,(x-1,y-1))])) (Jogador (x,y) Oeste b)      
                                           else Jogo (constroiMapa (removeCertaPeca (Caixa,(x,y-1)) (desconstroiMapa (p:ps)) ++ [(Caixa,(x+1,y-1))])) (Jogador (x,y) Este b) 
                            else Jogo (p:ps) (Jogador (x,y) d b) 

vazioEmbaixo :: (Int,Int)    -- ^Acumulador que nos permite manter a informação da coordenada em que vamos
                -> Jogo      -- ^Jogo
                -> Bool      -- ^Resultado
vazioEmbaixo (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioEmbaixo (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioEmbaixo (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2+1 = x == Vazio
 |otherwise = vazioEmbaixo (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b))

vazioEmcima :: (Int,Int)    -- ^Acumulador que nos permite manter a informação da coordenada em que vamos
                -> Jogo      -- ^Jogo
                -> Bool      -- ^Resultado
vazioEmcima (x1,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioEmcima (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioEmcima (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2-1 = x == Vazio
 |otherwise = vazioEmcima (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) d b))

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

podeTrepar :: (Int,Int)       -- ^Acumulador de coordenadas que nos permite verificar as peças nas coordenadas na diagonal e a frente do jogador
              -> Jogo         -- ^Jogo
              -> Bool         -- ^Resultado
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

podeTreparCaixa :: (Int,Int)          -- ^Acumulador de coordenadas que nos permite verificar as peças
                   -> Jogo            -- ^Jogo
                   -> Bool            -- ^Resultado
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

jogadorCai :: Jogo -> Jogo
jogadorCai (Jogo m (Jogador (x,y) d b))
 |vazioEmbaixo (0,0) (Jogo m (Jogador (x,y) d b)) = jogadorCai (Jogo m (Jogador (x,y+1) d b))
 |otherwise = Jogo m (Jogador (x,y) d b)

colocaCaixaCima :: (Int,Int) -> [(Peca,Coordenadas)] -> Jogo -> Jogo
colocaCaixaCima (x1,y1) l (Jogo ([]:t) (Jogador (x2,y2) d b)) = colocaCaixaCima (0,y1+1) l (Jogo t (Jogador (x2,y2) d b))
colocaCaixaCima (x1,y1) l (Jogo ((x:xs):t) (Jogador (x2,y2) d b))
 |x1 == x2 && y1 == y2-1 = Jogo (constroiMapa (l ++ [(Caixa,(x1,y1))])) (Jogador (x2,y2) d True)
 |otherwise = colocaCaixaCima (x1+1,y1) l (Jogo (xs:t) (Jogador (x2,y2) d b))

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

vazioFrente :: (Int,Int) -> Jogo -> Bool
vazioFrente _ (Jogo [] Jogador {}) = False
vazioFrente (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vazioFrente (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vazioFrente (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2 = x == Vazio
 |otherwise = vazioFrente (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
vazioFrente (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2 = x == Vazio
 |otherwise = vazioFrente (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))

vaiAtirar :: (Int,Int) -> Jogo -> Bool
vaiAtirar _ (Jogo [] Jogador {}) = False
vaiAtirar (_,y1) (Jogo ([]:t) (Jogador (x2,y2) d b)) = vaiAtirar (0,y1+1) (Jogo t (Jogador (x2,y2) d b))
vaiAtirar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Oeste b))
 |x1 == x2-1 && y1 == y2+1 = x == Vazio
 |otherwise = vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Oeste b))
vaiAtirar (x1,y1) (Jogo ((x:xs):t) (Jogador (x2,y2) Este b))
 |x1 == x2+1 && y1 == y2+1 = x == Vazio
 |otherwise = vaiAtirar (x1+1,y1) (Jogo (xs:t) (Jogador (x2,y2) Este b))

certaColuna :: Int -> (Int,Int) -> Mapa -> [(Peca,Coordenadas)]
certaColuna _ _ [] = []
certaColuna x1 (x2,y2) ([]:t) = certaColuna x1 (0,y2+1) t
certaColuna x1 (x2,y2) ((x:xs):t)
 |x1 == x2 = (x,(x2,y2)):certaColuna x1 (x2+1,y2) (xs:t)
 |otherwise = certaColuna x1 (x2+1,y2) (xs:t)

pecaCoordenada :: Int -> [(Peca,Coordenadas)] -> Int
pecaCoordenada y1 ((x,(x2,y2)):xs)
 |y1 == y2-1 && x == Vazio = pecaCoordenada (y1+1) xs
 |y1 == y2-1 && (x == Bloco || x == Caixa || x == Porta) = y1
 |otherwise = pecaCoordenada y1 xs

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos = foldl moveJogador

--random utilities
--[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
-- [[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],[Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]



