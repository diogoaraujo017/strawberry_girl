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
import Tarefa3_2021li1g097

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo (p:ps) (Jogador (x,y) d b)) m
{-----------------------------------/1/-----------------------------------}
 |m == AndarEsquerda = if not b
                       then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b))
                            then jogadorCair (0,0) (p:ps) (Jogo (p:ps) (Jogador (x-1,y) Oeste b))
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)
                         {-----------------------------/5/-----------------------------}
                       else if verificaLados (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b)) && naoBlocoCimaJogador (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b))
                            then Jogo (mapaAndarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x-1,y) Oeste b)
                            else Jogo (p:ps) (Jogador (x,y) Oeste b)

{-----------------------------------/1/-----------------------------------}
 |m == AndarDireita = if not b
                      then if verificaLados (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b))
                           then jogadorCair (0,0) (p:ps) (Jogo (p:ps) (Jogador (x+1,y) Este b))
                           else Jogo (p:ps) (Jogador (x,y) Este b)
                         {-----------------------------/5/-----------------------------}
                      else if verificaLados (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b)) && naoBlocoCimaJogador (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b))
                           then Jogo (mapaAndarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x+1,y) Este b)
                           else Jogo (p:ps) (Jogador (x,y) Este b)
{-----------------------------------/2/-----------------------------------}
 |m == Trepar = if not b
                then if verificaTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) && naoBlocoCimaJogador (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                     then case d of Oeste -> Jogo (p:ps) (Jogador (x-1,y-1) d b)
                                    Este -> Jogo (p:ps) (Jogador (x+1,y-1) d b)
                     else Jogo (p:ps) (Jogador (x,y) d b)
                  {-----------------------------/6/-----------------------------}
                else if verificaTrepar (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) && naoBlocoCimaJogador (0,0) (Jogo (p:ps) (Jogador (x,y-1) d b))
                     then case d of Oeste -> Jogo (mapaTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x-1,y+1) d b)
                                    Este -> Jogo (mapaTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x+1,y+1) d b)
                     else Jogo (p:ps) (Jogador (x,y) d b)
{-----------------------------------/3/-----------------------------------}
 |m == InterageCaixa = if not b
                                     {----------------------/3.1/----------------------}           {----------------------/3.2/----------------------}          {----------------------/3.3/----------------------}
                       then if verificaInterageCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) && naoBlocoCimaCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) && naoBlocoCimaJogador (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                            then Jogo (mapaPegarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x,y) d True)
                            else Jogo (p:ps) (Jogador (x,y) d b)
                         {--------------------------------/4/--------------------------------}
                       else if verificaInterageVazio (0,0) (Jogo (p:ps) (Jogador (x,y) d b)) 
                                                {----------------------/4.1/----------------------}
                            then Jogo (mapaPousarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x,y) d False)
                                                {----------------------/4.2/----------------------}
                            else if verificaInterageVazioCima (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
                                 then Jogo (mapaPousarCaixaCima (0,0) (Jogo (p:ps) (Jogador (x,y) d b))) (Jogador (x,y) d False)
                                 else Jogo (p:ps) (Jogador (x,y) d b)


{----------------------/Jogador a cair/----------------------}

jogadorCair :: (Int,Int) -> Mapa -> Jogo -> Jogo
jogadorCair  _ m (Jogo [] (Jogador (x,y) d b)) = Jogo m (Jogador (x,y) d b)
jogadorCair (a1,a2) m (Jogo ([]:ps) (Jogador (x,y) d b)) = jogadorCair (0,a2+1) m (Jogo ps (Jogador (x,y) d b))
jogadorCair (a1,a2) m (Jogo ((h:t):ps) (Jogador (x,y) d b))
 |a2==y+1 && a1==x = case h of Bloco -> Jogo m (Jogador (x,y) d b)
                               Caixa -> Jogo m (Jogador (x,y) d b)
                               Vazio -> jogadorCair (0,0) m (Jogo m (Jogador (x,y+1) d b))
                               Porta -> Jogo m (Jogador (x,y+1) d b)
 |otherwise = jogadorCair (a1+1,a2) m (Jogo (t:ps) (Jogador (x,y) d b))


jogadorCairCaixa :: (Int,Int) -> (Int,Int) -> Mapa -> Jogo -> Jogo
jogadorCairCaixa  _ _ m (Jogo [] (Jogador (x,y) d b)) = Jogo m (Jogador (x,y) d b)
jogadorCairCaixa (a1,a2) (c1,c2) m (Jogo ([]:ps) (Jogador (x,y) d b)) = jogadorCairCaixa (0,a2+1) (c1,c2) m (Jogo ps (Jogador (x,y) d b))
jogadorCairCaixa (a1,a2) (c1,c2) m (Jogo ((h:t):ps) (Jogador (x,y) d b))
 |a2==y+1 && a1==x = case h of Bloco -> Jogo m (Jogador (x,y) d b)
                               Caixa -> Jogo m (Jogador (x,y) d b)
                               Vazio -> jogadorCairCaixa (0,0) (c1,c2) m (Jogo m (Jogador (x,y+1) d b))
                               Porta -> Jogo m (Jogador (x,y+1) d b)
 |otherwise = jogadorCairCaixa (a1+1,a2) (c1,c2) m (Jogo (t:ps) (Jogador (x,y) d b))

{-----------------------------------/1/-----------------------------------}
verificaLados :: (Int,Int) -> Jogo -> Bool
verificaLados (_,_) (Jogo [] (Jogador (_,_) _ _ )) = False
verificaLados (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha1 (0,a2) p (Jogador (x,y) d b) || verificaLados (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                              where verificaLinha1 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                                    verificaLinha1 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                     |a2/=y = False
                                                                     |otherwise = if a1==x then case h of Bloco -> False
                                                                                                          Porta -> True
                                                                                                          Caixa -> False
                                                                                                          Vazio -> True
                                                                                           else verificaLinha1 (a1+1,a2) t (Jogador (x,y) d b)

{-----------------------------------/2/-----------------------------------}
verificaTrepar :: (Int,Int) -> Jogo -> Bool
verificaTrepar (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = case d of Oeste -> eBlocoOuCaixa (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b)) && vazioOuPortaEmCima (0,0) (Jogo (p:ps) (Jogador (x-1,y+1) d b))
                                                                     Este -> eBlocoOuCaixa (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b)) && vazioOuPortaEmCima (0,0) (Jogo (p:ps) (Jogador (x+1,y+1) d b))

eBlocoOuCaixa :: (Int,Int) -> Jogo -> Bool
eBlocoOuCaixa (_,_) (Jogo [] (Jogador (_,_) _ _ )) = False
eBlocoOuCaixa (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha1 (0,a2) p (Jogador (x,y) d b) || eBlocoOuCaixa (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                              where verificaLinha1 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                                    verificaLinha1 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                     |a2/=y = False
                                                                     |otherwise = if a1==x then case h of Bloco -> True
                                                                                                          Porta -> False
                                                                                                          Caixa -> True
                                                                                                          Vazio -> False
                                                                                           else verificaLinha1 (a1+1,a2) t (Jogador (x,y) d b)

vazioOuPortaEmCima :: (Int,Int) -> Jogo -> Bool
vazioOuPortaEmCima (_,_) (Jogo [] (Jogador (_,_) _ _ )) = True
vazioOuPortaEmCima (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha3 (0,a2) p (Jogador (x,y) d b) || vazioOuPortaEmCima (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                              where verificaLinha3 :: (Int,Int)-> [Peca] -> Jogador -> Bool
                                                    verificaLinha3 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                     |a2/=y = False
                                                     |otherwise = if a1==x then case h of Bloco -> False
                                                                                          Porta -> True
                                                                                          Caixa -> False
                                                                                          Vazio -> True
                                                                           else verificaLinha3 (a1+1,a2) t (Jogador (x,y) d b)

{-----------------------------------/3/-----------------------------------}
{----------------------/3.1/----------------------}
verificaInterageCaixa :: (Int,Int) -> Jogo -> Bool
verificaInterageCaixa (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = case d of Oeste -> eCaixa (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b))
                                                                            Este -> eCaixa (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b))

eCaixa :: (Int,Int) -> Jogo -> Bool
eCaixa (_,_) (Jogo [] (Jogador (_,_) _ _ )) = False
eCaixa (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha4 (0,a2) p (Jogador (x,y) d b) || eCaixa (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                              where verificaLinha4 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                                    verificaLinha4 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                     |a2/=y = False
                                                                     |otherwise = if a1==x then case h of Bloco -> False
                                                                                                          Porta -> False
                                                                                                          Caixa -> True
                                                                                                          Vazio -> False
                                                                                           else verificaLinha4 (a1+1,a2) t (Jogador (x,y) d b)

mapaPegarCaixa :: (Int,Int) -> Jogo -> Mapa
mapaPegarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Oeste = mapaPegarCaixaOeste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Este = mapaPegarCaixaEste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))

mapaPegarCaixaOeste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaPegarCaixaOeste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaPegarCaixaOeste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaPegarCaixaOeste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2/=y-1 && v2/=y = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |c2==y-1 = if c1==x then Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y = if v1==x-1 then Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

mapaPegarCaixaEste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaPegarCaixaEste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaPegarCaixaEste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaEste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaPegarCaixaOeste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaEste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaEste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaEste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2/=y-1 && v2/=y = h : constroiLinhaEste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |c2==y-1 = if c1==x then Caixa : constroiLinhaEste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaEste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y = if v1==x+1 then Vazio : constroiLinhaEste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaEste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

{----------------------/3.2/----------------------}

naoBlocoCimaCaixa :: (Int,Int) -> Jogo -> Bool
naoBlocoCimaCaixa (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = case d of Oeste -> naoBlocoCimaCaixaOeste (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b))
                                                                        Este -> naoBlocoCimaCaixaEste (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b))

naoBlocoCimaCaixaOeste :: (Int,Int) -> Jogo -> Bool
naoBlocoCimaCaixaOeste (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha4 (0,a2) p (Jogador (x,y) d b) || naoBlocoCimaCaixaOeste (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                                     where verificaLinha4 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                                           verificaLinha4 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                            |a2/=y-1 = False
                                                                            |otherwise = if a1==x then case h of Bloco -> False
                                                                                                                 Porta -> False
                                                                                                                 Caixa -> False
                                                                                                                 Vazio -> True
                                                                                                  else verificaLinha4 (a1+1,a2) t (Jogador (x,y) d b)

naoBlocoCimaCaixaEste :: (Int,Int) -> Jogo -> Bool
naoBlocoCimaCaixaEste (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha4 (0,a2) p (Jogador (x,y) d b) || naoBlocoCimaCaixaEste (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                                     where verificaLinha4 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                                           verificaLinha4 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                            |a2/=y-1 = False
                                                                            |otherwise = if a1==x then case h of Bloco -> False
                                                                                                                 Porta -> False
                                                                                                                 Caixa -> False
                                                                                                                 Vazio -> True
                                                                                                  else verificaLinha4 (a1+1,a2) t (Jogador (x,y) d b)

{----------------------/3.3/----------------------}

naoBlocoCimaJogador :: (Int,Int) -> Jogo -> Bool
naoBlocoCimaJogador (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha4 (0,a2) p (Jogador (x,y) d b) || naoBlocoCimaJogador (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                                     where verificaLinha4 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                                           verificaLinha4 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                            |a2/=y-1 = False
                                                                            |otherwise = if a1==x then case h of Bloco -> False
                                                                                                                 Porta -> False
                                                                                                                 Caixa -> False
                                                                                                                 Vazio -> True
                                                                                                  else verificaLinha4 (a1+1,a2) t (Jogador (x,y) d b)

{-----------------------------------/4/-----------------------------------}
{----------------------/4.1/----------------------}
verificaInterageVazio :: (Int,Int) -> Jogo -> Bool
verificaInterageVazio (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = case d of Oeste -> eVazio (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b))
                                                                            Este -> eVazio (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b))

eVazio :: (Int,Int) -> Jogo -> Bool
eVazio (_,_) (Jogo [] (Jogador (_,_) _ _ )) = False
eVazio (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = verificaLinha5 (0,a2) p (Jogador (x,y) d b) || eVazio (0,a2+1) (Jogo ps (Jogador (x,y) d b))
                                                         where verificaLinha5 :: (Int,Int) -> [Peca] -> Jogador -> Bool
                                                               verificaLinha5 (a1,a2) (h:t) (Jogador (x,y) d b)
                                                                |a2/=y = False
                                                                |otherwise = if a1==x then case h of Bloco -> False
                                                                                                     Porta -> False
                                                                                                     Caixa -> False
                                                                                                     Vazio -> True
                                                                                           else verificaLinha5 (a1+1,a2) t (Jogador (x,y) d b)

mapaPousarCaixa :: (Int,Int) -> Jogo -> Mapa
mapaPousarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Oeste = mapaPousarCaixaOeste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Este = mapaPousarCaixaEste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))

mapaPousarCaixaOeste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaPousarCaixaOeste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaPousarCaixaOeste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaPousarCaixaOeste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2/=y && v2/=y-1 = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |c2==y = if c1==x-1 then Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 = if v1==x then Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

mapaPousarCaixaEste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaPousarCaixaEste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaPousarCaixaEste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaPousarCaixaEste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2/=y && v2/=y-1 = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |c2==y = if c1==x+1 then Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 = if v1==x then Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)


{----------------------/4.2/----------------------}

verificaInterageVazioCima :: (Int,Int) -> Jogo -> Bool
verificaInterageVazioCima (a1,a2) (Jogo (p:ps) (Jogador (x,y) d b)) = case d of Oeste -> eVazio (0,0) (Jogo (p:ps) (Jogador (x-1,y-1) d b)) && eBlocoOuCaixa (0,0) (Jogo (p:ps) (Jogador (x-1,y) d b))
                                                                                Este -> eVazio (0,0) (Jogo (p:ps) (Jogador (x+1,y-1) d b)) && eBlocoOuCaixa (0,0) (Jogo (p:ps) (Jogador (x+1,y) d b))

mapaPousarCaixaCima :: (Int,Int) -> Jogo -> Mapa
mapaPousarCaixaCima (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Oeste = mapaPousarCaixaOeste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Este = mapaPousarCaixaEste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))

mapaPousarCaixaOesteCima :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaPousarCaixaOesteCima (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaPousarCaixaOesteCima (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaPousarCaixaOesteCima (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2/=y-2 && v2/=y-1 = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |c2==y-2 = if c1==x-1 then Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 = if v1==x then Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

mapaPousarCaixaEsteCima :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaPousarCaixaEsteCima (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaPousarCaixaEsteCima (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaPousarCaixaEsteCima (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2/=y-2 && v2/=y-1 = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |c2==y-2 = if c1==x+1 then Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 = if v1==x then Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                                                   else h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)


{-----------------------------------/5/-----------------------------------}

mapaAndarCaixa :: (Int,Int) -> Jogo -> Mapa
mapaAndarCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Oeste = mapaAndarCaixaOeste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Este = mapaAndarCaixaEste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))

mapaAndarCaixaOeste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaAndarCaixaOeste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaAndarCaixaOeste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaAndarCaixaOeste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2==y-1 && c1==x-1 = Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 && v1==x = Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |otherwise = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

mapaAndarCaixaEste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaAndarCaixaEste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaAndarCaixaEste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaAndarCaixaOeste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2==y-1 && c1==x+1 = Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 && v1==x = Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |otherwise = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

{-----------------------------------/6/-----------------------------------}

mapaTreparCaixa :: (Int,Int) -> Jogo -> Mapa
mapaTreparCaixa (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Oeste = mapaTreparCaixaOeste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))
 |d==Este = mapaTreparCaixaEste (0,0) (0,0) (Jogo (p:ps) (Jogador (x,y) d b))

mapaTreparCaixaOeste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaTreparCaixaOeste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaTreparCaixaOeste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaTreparCaixaOeste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2==y-2 && c1==x-1 = Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 && v1==x = Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |otherwise = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)

mapaTreparCaixaEste :: (Int,Int) -> (Int,Int) -> Jogo -> Mapa
mapaTreparCaixaEste (_,_) (_,_) (Jogo [] (Jogador (x,y) d b)) = []
mapaTreparCaixaEste (c1,c2) (v1,v2) (Jogo (p:ps) (Jogador (x,y) d b)) = constroiLinhaOeste (0,c2) (0,v2) p (Jogador (x,y) d b) : mapaTreparCaixaEste (0,c2+1) (0,v2+1) (Jogo ps (Jogador (x,y) d b))
                                                                        where constroiLinhaOeste :: (Int,Int) -> (Int,Int) -> [Peca] -> Jogador -> [Peca]
                                                                              constroiLinhaOeste (_,_) (_,_) [] _ = []
                                                                              constroiLinhaOeste (c1,c2) (v1,v2) (h:t) (Jogador (x,y) d b)
                                                                               |c2==y-2 && c1==x+1 = Caixa : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |v2==y-1 && v1==x = Vazio : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)
                                                                               |otherwise = h : constroiLinhaOeste (c1+1,c2) (v1+1,v2) t (Jogador (x,y) d b)



{-

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j
correrMovimentos j (h:t) = correrMovimentos (moveJogador j h) t

-}
-- [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

-- [[Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],[Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],[Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],[Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],[Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]]
