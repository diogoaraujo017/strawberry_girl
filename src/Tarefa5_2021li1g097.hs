{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- |
Module      : Tarefa5_2021li1g097
Description : Aplicação Gráfica
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Tarefa5_2021li1g097 where

import Mapas ( level1,level2,level3,level4,level5,level6,level7,level8,level9,level10,player1,player2,player3,player4,player5,player6,player7,player8,player9,player10 )
import LI12122 ( Jogo (Jogo), Jogador (Jogador), Peca (Porta, Bloco), Mapa, Movimento (AndarDireita, AndarEsquerda, Trepar, InterageCaixa), Peca(Vazio), Peca(Caixa), Direcao (Oeste) )
import Tarefa1_2021li1g097 ()
import Tarefa2_2021li1g097 ()
import Tarefa3_2021li1g097 (mostrarJogoAux)
import Tarefa4_2021li1g097 (moveJogador)
import Graphics.Gloss( white,play,loadBMP,Display(InWindow, FullScreen),Picture(Circle, Scale, Pictures, Text, Color, Translate), makeColor, azure, makeColorI, bitmap )
import Graphics.Gloss.Interface.Pure.Game ( Key(SpecialKey),KeyState(Down),SpecialKey(KeyRight, KeyLeft, KeyDown, KeyUp, KeyEnter, KeyF5, KeyBackspace, KeyEsc, KeySpace),Event(EventKey) )
import Graphics.Gloss.Interface.Pure.Simulate ( Picture(Polygon) )
import System.Console.Terminfo (restoreDefaultColors, Color)
import Graphics.Gloss.Raster.Array
import Graphics.Rendering.OpenGL (drawElementsInstanced)

data Opcoes = Jogar
            | Mapas
            | Controls
            | Sair
            deriving (Eq)

data Opcoes1 = Next
             | Menu
             deriving (Eq)

data Opcoes2 = Menu1
             | Sair1
             deriving (Eq)

data Opcoes3 = Menu3
             | Continuar
             deriving (Eq)

data Opcoes4 = Continuar1
             | Novo
             | Mapas1
             | Controls2
             | Sair2
            deriving (Eq)

data Mapas = Level1
           | Level2
           | Level3
           | Level4
           | Level5
           | Level6
           | Level7
           | Level8
           | Level9
           | Level10
           | Menu2
          deriving (Eq)

data Menu = Controlador Opcoes
          | Mapa Mapas Bool Mapas
          | Jogando Mapas
          | VenceuJogo Opcoes1 Mapas
          | Controls3 Bool Mapas
          | VenceuUltimoJogo Opcoes2
          | Pausa Opcoes3 Mapas
          | Controlador2 Opcoes4 Mapas
          deriving (Eq)

type Imagens = [Picture]

type World = (Menu, Jogo, Imagens)

{- | A varíavel 'window' guarda o valor do nosso Display

== Código:
@
window :: Display
window = FullScreen
@
-}

window :: Display
window = FullScreen

{- | A varíavel 'fr' guarda o valor do número de simulações realizadas em um segundo.

== Código:
@
fr :: Int
fr = 60
@
-}
fr :: Int
fr = 60

{- | A função 'drawWorld' é uma função que recebe um 'World' e que a partir da informação que recebe desse dito 'World' desenha o que nos visualizamos no ecrã, desde os menus iniciais ao próprio jogo em si.

== Código:
@
drawWorld :: World -> Picture
--------- Desenha o menu quando o jogador venceu o ultimo jogo
drawWorld (VenceuUltimoJogo Menu1, jogo, imagens) = Pictures [Scale 2.2 2.2   (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))))))))))))))] -------
drawWorld (VenceuUltimoJogo Sair1, jogo, imagens) = Pictures [Scale 2.2 2.2   (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))))))))))))))] --------
--------- Desenha o menu quando o jogador venceu o jogo
drawWorld (VenceuJogo Next l, jogo, imagens) = Pictures [Scale 2.2 2.2   (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))))))))))))]--------- 
drawWorld (VenceuJogo Menu l, jogo, imagens) = Pictures [Scale 2.2 2.2   (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))))))))))))] ----------
--------- Desenha o menu pausa
drawWorld (Pausa Continuar l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))]
drawWorld (Pausa Menu3 l, jogo, imagens) = Pictures [Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))]
--------- Desenha o menu inicial 2
drawWorld (Controlador2 Continuar1 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))] 
drawWorld (Controlador2 Novo l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))]  
drawWorld (Controlador2 Mapas1 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))] 
drawWorld (Controlador2 Sair2 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))] 
drawWorld (Controlador2 Controls2 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))]
--------- Desenha o menu dos mapas 
drawWorld (Mapa Level1 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))]
drawWorld (Mapa Level2 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))] 
drawWorld (Mapa Level3 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))] 
drawWorld (Mapa Level4 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))]
drawWorld (Mapa Level5 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))))] 
drawWorld (Mapa Level6 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))))]
drawWorld (Mapa Level7 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))))))]
drawWorld (Mapa Level8 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))))))]
drawWorld (Mapa Level9 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))))))))]
drawWorld (Mapa Level10 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))))))))]
drawWorld (Mapa Menu2 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2   (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))))))))))))))))))))]
--------- Desenha o menu inicial 
drawWorld (Controlador Jogar, jogo, imagens) = Pictures [ Scale 2.2 2.2 (head(tail(tail(tail(tail(tail(tail imagens)))))))] 
drawWorld (Controlador Mapas, jogo, imagens) = Pictures [ Scale 2.2 2.2 (head(tail(tail(tail(tail(tail(tail(tail imagens))))))))]
drawWorld (Controlador Controls, jogo, imagens) = Pictures [ Scale 2.2 2.2 (head(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))]
drawWorld (Controlador Sair, jogo, imagens) = Pictures [ Scale 2.2 2.2  (head(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens))))))))))]
--------- Desenha o menu dos controlos
drawWorld (Controls3 b m, jogo, imagens) = Pictures [Scale 2.2 2.2 (head(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail(tail imagens)))))))))))))))))))))))))))))] -------------------------
--------- Desenha o jogando
drawWorld (Jogando l, jogo, imagens) 
  | l == Level1 = Translate 90 (-250) $ Scale 1.4 1.4 (Pictures drawing)
  | l == Level2 = Translate 40 (-150) $ Scale 1.3 1.3 (Pictures drawing)
  | l == Level3 = Translate 125 (-105) $ Scale 1.3 1.3 (Pictures drawing)
  | l == Level4 = Translate 0 15 $ Scale 1.1 1.1 (Pictures drawing)
  | l == Level5 = Translate 0 15 $ Scale 1.1 1.1 (Pictures drawing)
  | l == Level6 = Translate 35 (-20) $ Scale 1.2 1.2 (Pictures drawing)
  | l == Level7 = Translate (-70) (-15) $ Scale 1.1 1.1 (Pictures drawing)
  | l == Level8 = Translate (-145) 100 $ Scale 0.9 0.9  (Pictures drawing)
  | l == Level9 = Translate (-140) 140 $ Scale 0.8 0.8 (Pictures drawing)
  | l == Level10 = Translate (-180) 140 $ Scale 0.8 0.8 (Pictures drawing)
  where
      drawing = drawingMap ++ [drawingPlayer]
      drawingMap
        = drawMap
             c a (getMapa (Jogando l, jogo, imagens)) imagens
      drawingPlayer
        = drawPlayer (getJogador (Jogando l, jogo, imagens)) imagens
@
-}
drawWorld :: World -> Picture
--------- Desenha o menu quando o jogador venceu o ultimo jogo
drawWorld (VenceuUltimoJogo Menu1, jogo, imagens) = Pictures [Scale 2.2 2.2 (imagens !! 31)] -------
drawWorld (VenceuUltimoJogo Sair1, jogo, imagens) = Pictures [Scale 2.2 2.2 (imagens !! 32)] --------
--------- Desenha o menu quando o jogador venceu o jogo
drawWorld (VenceuJogo Next l, jogo, imagens) = Pictures [Scale 2.2 2.2 (imagens !! 30)]--------- 
drawWorld (VenceuJogo Menu l, jogo, imagens) = Pictures [Scale 2.2 2.2 (imagens !! 29)] ----------
--------- Desenha o menu pausa
drawWorld (Pausa Continuar l, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 16)]
drawWorld (Pausa Menu3 l, jogo, imagens) = Pictures [Scale 2.2 2.2  (imagens !! 15)]
--------- Desenha o menu inicial 2
drawWorld (Controlador2 Continuar1 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (imagens !! 10)]
drawWorld (Controlador2 Novo l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (imagens !! 11)]
drawWorld (Controlador2 Mapas1 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (imagens !! 12)]
drawWorld (Controlador2 Sair2 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (imagens !! 14)]
drawWorld (Controlador2 Controls2 l, jogo, imagens) = Pictures [ Scale 2.2 2.2  (imagens !! 13)]
--------- Desenha o menu dos mapas 
drawWorld (Mapa Level1 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 17)]
drawWorld (Mapa Level2 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 18)] 
drawWorld (Mapa Level3 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 19)] 
drawWorld (Mapa Level4 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 20)]
drawWorld (Mapa Level5 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 21)] 
drawWorld (Mapa Level6 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 22)]
drawWorld (Mapa Level7 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 23)]
drawWorld (Mapa Level8 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 24)]
drawWorld (Mapa Level9 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 25)]
drawWorld (Mapa Level10 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 26)]
drawWorld (Mapa Menu2 b m, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 27)]
--------- Desenha o menu inicial 
drawWorld (Controlador Jogar, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 6)]
drawWorld (Controlador Mapas, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 7)]
drawWorld (Controlador Controls, jogo, imagens) = Pictures [ Scale 2.2 2.2 (imagens !! 8)]
drawWorld (Controlador Sair, jogo, imagens) = Pictures [ Scale 2.2 2.2  (imagens !! 9)]
--------- Desenha o menu dos controlos
drawWorld (Controls3 b m, jogo, imagens) = Pictures [Scale 2.2 2.2 (imagens !! 28)] -------------------------
--------- Desenha o jogando
drawWorld (Jogando l, jogo, imagens) 
  | l == Level1 = Translate 90 (-250) $ Scale 1.4 1.4 (Pictures drawing)
  | l == Level2 = Translate 40 (-150) $ Scale 1.3 1.3 (Pictures drawing)
  | l == Level3 = Translate 125 (-105) $ Scale 1.3 1.3 (Pictures drawing)
  | l == Level4 = Translate 0 15 $ Scale 1.1 1.1 (Pictures drawing)
  | l == Level5 = Translate 0 15 $ Scale 1.1 1.1 (Pictures drawing)
  | l == Level6 = Translate 35 (-20) $ Scale 1.2 1.2 (Pictures drawing)
  | l == Level7 = Translate (-70) (-15) $ Scale 1.1 1.1 (Pictures drawing)
  | l == Level8 = Translate (-145) 100 $ Scale 0.9 0.9  (Pictures drawing)
  | l == Level9 = Translate (-140) 140 $ Scale 0.8 0.8 (Pictures drawing)
  | l == Level10 = Translate (-180) 140 $ Scale 0.8 0.8 (Pictures drawing)
  where
      drawing = drawingMap ++ [drawingPlayer]
      drawingMap
        = drawMap
             c a (getMapa (Jogando l, jogo, imagens)) imagens
      drawingPlayer
        = drawPlayer (getJogador (Jogando l, jogo, imagens)) imagens

{- | A função 'drawLine' é a função que utiliza como função auxiliar a função 'drawPiece' e que escreve numa lista 
de Pictures, que consiste nas imagens das peças do jogo e as coordenadas onde essas peças vão ser mostradas no ecrã que juntas 
formam uma linha do mapa do jogo.

== Código:
@
drawLine :: Float -> Float -> [Peca] -> Imagens -> [Picture]
drawLine x y (h:t) imagens = peca : resto
  where peca = drawPiece x y h imagens
        resto = drawLine (x+l) y t imagens
drawLine _ _ _ _ = []
@
-}
drawLine :: Float -> Float -> [Peca] -> Imagens -> [Picture]
drawLine x y (h:t) imagens = peca : resto
  where peca = drawPiece x y h imagens
        resto = drawLine (x+l) y t imagens
drawLine _ _ _ _ = []

{- | A função 'drawPiece' é a função que vai através de um valor x e um valor y criar uma picture com a imagem da peça que é suposto ser colocada nessas coordenadas.

== Código:
@
drawPiece :: Float -> Float -> Peca -> Imagens -> Picture
drawPiece x y peca imagens = Translate x y imagem
  where imagem = whatImg peca imagens
@
-}
drawPiece :: Float -> Float -> Peca -> Imagens -> Picture
drawPiece x y peca imagens = Translate x y imagem
  where imagem = whatImg peca imagens

{- | A função 'whatImg' vai descobrir qual a imagem a que uma certa peça corresponde numa lista de imagens que foram predefinidas.

== Código:
@
whatImg :: Peca  -> Imagens -> Picture
whatImg x l
 |x == Vazio = head (tail (tail (tail (tail (tail l)))))
 |x == Bloco  = head (tail (tail (tail (tail l))))
 |x == Porta  = head l
 |x == Caixa = head (tail (tail (tail l)))
 |otherwise = undefined
@
-}
whatImg :: Peca  -> Imagens -> Picture
whatImg x l
 |x == Vazio = head (tail (tail (tail (tail (tail l)))))
 |x == Bloco  = head (tail (tail (tail (tail l))))
 |x == Porta  = head l
 |x == Caixa = head (tail (tail (tail l)))
 |otherwise = undefined

{- | A função 'drawMap' é a função que utiliza como função auxiliar a função 'drawLine' e que escreve numa lista 
de Pictures, que consistem nas imagens das peças do jogo e as coordenadas onde essas peças vão ser mostradas no ecrã que juntas 
formam um mapa do jogo.

== Código:
@
drawMap :: Float -> Float -> Mapa  -> Imagens -> [Picture]
drawMap x y (h:t) imagens = linha ++ resto
   where linha = drawLine x y h imagens
         resto = drawMap x (y-l) t imagens
drawMap _ _ _ _ = []
@
-}
drawMap :: Float -> Float -> Mapa  -> Imagens -> [Picture]
drawMap x y (h:t) imagens = linha ++ resto
   where linha = drawLine x y h imagens
         resto = drawMap x (y-l) t imagens
drawMap _ _ _ _ = []

{- | A função 

== Código:
@
drawPlayer :: Jogador -> Imagens -> Picture
drawPlayer (Jogador (x,y) d b) imagens
 |d == Oeste = Translate (realPlayerX x-64) (realPlayerY y) (head (tail (tail imagens)))
 |otherwise = Translate  (realPlayerX x-64) (realPlayerY y) (head (tail imagens))
@
-}
drawPlayer :: Jogador -> Imagens -> Picture
drawPlayer (Jogador (x,y) d b) imagens
 |d == Oeste = Translate (realPlayerX x-64) (realPlayerY y) (head (tail (tail imagens)))
 |otherwise = Translate  (realPlayerX x-64) (realPlayerY y) (head (tail imagens))

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
realPlayerX :: Int -> Float
realPlayerX = (+ c).(* l).realToFrac.succ

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
realPlayerY :: Int -> Float
realPlayerY = (+ a).(* (-l)).realToFrac

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
a :: Float
a = 400

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
c :: Float
c = -670.0

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
getMapa :: World -> Mapa
getMapa (_,Jogo m j,_) =  m

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
getJogador :: World -> Jogador
getJogador (_,Jogo m j,_) = j

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
event :: Event -> World -> World
-------------- Menu Inicial 2
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Continuar1 l, jogo, imagens) = (Jogando l, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Novo l, jogo, imagens) = (Jogando Level1, Jogo level1 player1 , imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Mapas1 l, jogo, imagens) = (Mapa Level1 True l, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Sair2 l, jogo, imagens) = undefined
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Controls2 l, jogo, imagens) = (Controls3 True l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Continuar1 l, jogo, imagens) = (Controlador2 Sair2 l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Sair2 l, jogo, imagens) = (Controlador2 Controls2 l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Mapas1 l, jogo, imagens) = (Controlador2 Novo l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Novo l, jogo, imagens) = (Controlador2 Continuar1 l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Controls2 l, jogo, imagens) = (Controlador2 Mapas1 l, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Controls2 l, jogo, imagens) = (Controlador2 Sair2 l, jogo, imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Controlador2 Continuar1 l, jogo, imagens) = (Controlador2 Novo l, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Novo l, jogo, imagens) = (Controlador2 Mapas1 l, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Mapas1 l, jogo, imagens) = (Controlador2 Controls2 l, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Sair2 l, jogo, imagens) = (Controlador2 Continuar1 l, jogo, imagens)
-------------- Menu Pausa
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Menu3 l, jogo , imagens) = (Controlador2 Continuar1 l, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Continuar l, jogo, imagens) = (Jogando l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa Menu3 l, jogo , imagens) = (Pausa Continuar l, jogo, imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Continuar l, jogo, imagens) = (Pausa Menu3 l, jogo , imagens)
-------------- Menu Inicial
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, Jogo l p, imagens) = (Jogando Level1 , Jogo level1 player1 , imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imagens) = (Controlador Sair, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imagens) = (Controlador Mapas, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Mapas, jogo, imagens) = (Controlador Controls, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Controls, jogo, imagens) = (Controlador Sair, jogo, imagens)
event (EventKey (SpecialKey KeyEnter ) Down _ _) (Controlador Controls, jogo, imagens) = (Controls3 False Level1, jogo, imagens)
event (EventKey (SpecialKey KeyEnter ) Down _ _) (Controlador Mapas, jogo, imagens) = (Mapa Level1 False Level1, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Mapas, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Controls, jogo, imagens) = (Controlador Mapas, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imagens) = (Controlador Controls, jogo, imagens) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imagens) = undefined  
------------- Menu quando venceu um jogo (não sendo o último)
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Next l, jogo, imagens) = (Jogando (nextMundo l), Jogo (nextMap l) (nextPlayer l), imagens) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu Level5, jogo, imagens) = (Controlador Jogar, jogo, imagens) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Next l, jogo, imagens) = (VenceuJogo Menu l, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (VenceuJogo Next l, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (VenceuJogo Next l, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Next l, jogo, imagens) = (VenceuJogo Menu l, jogo, imagens)
------------- Menu Mapas
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level5 b m, jogo, imagens) = (Mapa Menu2 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Mapa Level1 b m, jogo, imagens) = (Mapa Level2 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level2 b m, jogo, imagens) = (Mapa Level3 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level3 b m, jogo, imagens) = (Mapa Level4 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level4 b m, jogo, imagens) = (Mapa Level5 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Menu2 b m, jogo, imagens) = (Mapa Menu2 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Level1 b m, jogo, imagens) = (Mapa Level1 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level5 b m, jogo, imagens) = (Mapa Level4 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level4 b m, jogo, imagens) = (Mapa Level3 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level3 b m, jogo, imagens) = (Mapa Level2 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level2 b m, jogo, imagens) = (Mapa Level1 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Menu2 b m, jogo, imagens) = (Mapa Level5 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level6 b m, jogo, imagens) = (Mapa Level7 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Mapa Level7 b m, jogo, imagens) = (Mapa Level8 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level8 b m, jogo, imagens) = (Mapa Level9 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level9 b m, jogo, imagens) = (Mapa Level10 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level10 b m, jogo, imagens) = (Mapa Menu2 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Level6 b m, jogo, imagens) = (Mapa Level6 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level7 b m, jogo, imagens) = (Mapa Level6 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level8 b m, jogo, imagens) = (Mapa Level7 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level9 b m, jogo, imagens) = (Mapa Level8 b m, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level10 b m, jogo, imagens) = (Mapa Level9 b m, jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level6 b m, jogo, imagens) = (Mapa Level6 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level7 b m, jogo, imagens) = (Mapa Level7 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level8 b m, jogo, imagens) = (Mapa Level8 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level9 b m, jogo, imagens) = (Mapa Level9 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyRight ) Down _ _) (Mapa Level10 b m, jogo, imagens) = (Mapa Level10 b m, jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level1 b m, jogo, imagens) = (Mapa Level6 b m, jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level2 b m, jogo, imagens) = (Mapa Level7 b m, jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level3 b m, jogo, imagens) = (Mapa Level8 b m, jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level4 b m, jogo, imagens) = (Mapa Level9 b m, jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level5 b m, jogo, imagens) = (Mapa Level10 b m, jogo, imagens)
event (EventKey (SpecialKey KeyLeft ) Down _ _) (Mapa Level6 b m, jogo, imagens) = (Mapa Level1 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level7 b m, jogo, imagens) = (Mapa Level2 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level8 b m, jogo, imagens) = (Mapa Level3 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level9 b m, jogo, imagens) = (Mapa Level4 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyLeft ) Down _ _) (Mapa Level10 b m, jogo, imagens) = (Mapa Level5 b m, jogo,  imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level1 b m, jogo, imagens) = (Mapa Level1 b m, jogo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level2 b m, jogo, imagens) = (Mapa Level2 b m, jogo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level3 b m, jogo, imagens) = (Mapa Level3 b m, jogo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level4 b m, jogo, imagens) = (Mapa Level4 b m, jogo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level5 b m, jogo, imagens) = (Mapa Level5 b m, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Menu2 b m, jogo, imagens) = if b then (Controlador2 Continuar1 m, jogo, imagens) else (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa l b m, jogo, imagens) = (Jogando l, Jogo (whatMap l) (whatPlayer l), imagens)
-------------- Jogando
event (EventKey (SpecialKey KeySpace ) Down _ _) (Jogando l, jogo, imagens) = (Pausa Continuar l, jogo , imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo Trepar, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo AndarDireita , imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo AndarEsquerda , imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo InterageCaixa , imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, Jogo (whatMap l) (whatPlayer l) , imagens)
-------------- Menu Controlos
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controls3 b l, jogo, imagens) = if b then (Controlador2 Continuar1 l, jogo, imagens) else (Controlador Jogar, jogo, imagens)
-------------- Venceu o ultimo jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuUltimoJogo Menu1, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuUltimoJogo Sair1, jogo, imagens) = undefined
event (EventKey (SpecialKey KeyUp ) Down _ _) (VenceuUltimoJogo Sair1, jogo, imagens) = (VenceuUltimoJogo Menu1, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuUltimoJogo Menu1, jogo, imagens) = (VenceuUltimoJogo Sair1, jogo, imagens)
event _ w = w 

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
coordPorta :: (Int,Int) ->  Jogo -> Bool
coordPorta (x1,y1) (Jogo ([]:ps) (Jogador (x2,y2) d b)) = coordPorta (0,y1+1) (Jogo ps (Jogador (x2,y2) d b))
coordPorta (x1,y1) (Jogo ((h:t):ps) (Jogador (x2,y2) d b))
 |h == Porta && x1 == x2 && y1 == y2 = True
 |otherwise = coordPorta (x1+1,y1) (Jogo (t:ps) (Jogador (x2,y2) d b))
coordPorta (x1,y1) (Jogo [] (Jogador (x2,y2) d b)) = False

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
whatMap :: Mapas -> Mapa
whatMap m
 |m == Level1 = level1
 |m == Level2 = level2
 |m == Level3 = level3
 |m == Level4 = level4
 |m == Level5 = level5
 |m == Level6 = level6
 |m == Level7 = level7
 |m == Level8 = level8
 |m == Level9 = level9
 |m == Level10 = level10

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
whatPlayer :: Mapas -> Jogador
whatPlayer p
 |p == Level1 = player1
 |p == Level2 = player2
 |p == Level3 = player3
 |p == Level4 = player4
 |p == Level5 = player5
 |p == Level6 = player6
 |p == Level7 = player7
 |p == Level8 = player8
 |p == Level9 = player9
 |p == Level10 = player10

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
nextMundo :: Mapas -> Mapas
nextMundo l
 |l == Level1 = Level2
 |l == Level2 = Level3
 |l == Level3 = Level4
 |l == Level4 = Level5
 |l == Level5 = Level6
 |l == Level6 = Level7
 |l == Level7 = Level8
 |l == Level8 = Level9
 |l == Level9 = Level10
nextMundo _ = undefined

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
nextMap :: Mapas -> Mapa
nextMap l
 |l == Level1 = level2
 |l == Level2 = level3
 |l == Level3 = level4
 |l == Level4 = level5
 |l == Level5 = level6
 |l == Level6 = level7
 |l == Level7 = level8
 |l == Level8 = level9
 |l == Level9 = level10
 |otherwise = undefined

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
nextPlayer :: Mapas -> Jogador
nextPlayer l
 | l == Level1 = player2
 | l == Level2 = player3
 | l == Level3 = player4
 | l == Level4 = player5
 | l == Level5 = player6
 | l == Level6 = player7
 | l == Level7 = player8
 | l == Level8 = player9
 | l == Level9 = player10
 |otherwise = undefined

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
l :: Float
l = 64.0

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
updateWorld :: Float -> World -> World
updateWorld _ (Jogando l, Jogo m (Jogador (x,y) d b) , imagens) = if coordPorta (0,0) (Jogo m (Jogador (x,y) d b))
                                                                     then if l == Level10                   
                                                                          then (VenceuUltimoJogo Menu1, Jogo m (Jogador (x,y) d b), imagens) 
                                                                          else (VenceuJogo Next l, Jogo m (Jogador (x,y) d b), imagens)
                                                                     else (Jogando l, Jogo m (Jogador (x,y) d b), imagens) 
updateWorld _ w = w

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
cor :: Graphics.Gloss.Raster.Array.Color
cor = makeColorI 0 25 51 0

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
vazia :: Picture
vazia = Color cor $ Polygon [(0,0),(0,l),(l,0),(l,l)]

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
main :: IO ()
main = do
         blockdude_direita <- loadBMP "img/right1.bmp"
         blockdude_esquerda <- loadBMP "img/left1.bmp"
         porta <- loadBMP "img/porta.bmp"
         caixa <- loadBMP "img/caixa.bmp"
         bloco <- loadBMP "img/bloco.bmp"
         play1 <- loadBMP "img/play.bmp"
         maps <- loadBMP "img/maps.bmp"
         controls <- loadBMP "img/controls.bmp"
         exit <- loadBMP "img/exit.bmp"
         resume <- loadBMP "img/resume.bmp"
         newgame <- loadBMP "img/new_game.bmp"
         maps2 <- loadBMP "img/maps2.bmp"
         controls2 <- loadBMP "img/controls2.bmp"
         exit2 <- loadBMP "img/exit2.bmp"
         menu4 <- loadBMP "img/menu4.bmp" 
         resume4 <- loadBMP "img/resume4.bmp" 
         mlv <- loadBMP "img/menulvl.bmp"
         lv9 <- loadBMP "img/level9.bmp"
         lv10 <- loadBMP "img/lvel10.bmp"
         lv8 <- loadBMP "img/level8.bmp"
         lv7 <- loadBMP "img/level7.bmp"
         lv6 <- loadBMP "img/level6.bmp"
         lv5 <- loadBMP "img/level5.bmp"
         lv3 <- loadBMP "img/level3.bmp" 
         lv2 <- loadBMP "img/level2.bmp" 
         lv1 <- loadBMP "img/level1.bmp" 
         lv4 <- loadBMP "img/level4.bmp" 
         con <- loadBMP "img/cntrpage.bmp" 
         men <- loadBMP "img/menu3.bmp" 
         nx <- loadBMP "img/next3.bmp" 
         mn <- loadBMP "img/menu5.bmp" 
         ex <- loadBMP "img/exit5.bmp" 

         let imagens = [porta,blockdude_direita,blockdude_esquerda,caixa,bloco,
                        vazia,play1,maps,controls,exit,resume,newgame,maps2,controls2,exit2,menu4,resume4,
                        lv1,lv2,lv3,lv4,lv5,lv6,lv7,lv8,lv9,lv10,mlv,con,men,nx,mn,ex]

         let inicialState = (Controlador Jogar, Jogo level1 player1, imagens)

         play window cor fr inicialState drawWorld event updateWorld