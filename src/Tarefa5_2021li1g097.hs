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

import Mapas
    ( level1,
      level2,
      level3,
      level4,
      level5,
      level6,
      level7,
      level8,
      level9,
      level10,
      player1,
      player2,
      player3,
      player4,
      player5,
      player6,
      player7,
      player8,
      player9,
      player10 )
import LI12122 ( Jogo (Jogo), Jogador (Jogador), Peca (Porta, Bloco), Mapa, Movimento (AndarDireita, AndarEsquerda, Trepar, InterageCaixa), Peca(Vazio), Peca(Caixa), Direcao (Oeste) )
import Tarefa1_2021li1g097 ()
import Tarefa2_2021li1g097 ()
import Tarefa3_2021li1g097 (mostrarJogoAux)
import Tarefa4_2021li1g097 (moveJogador)
import Graphics.Gloss
    ( blue,
      green,
      red,
      white,
      play,
      loadBMP,
      Display(InWindow, FullScreen),
      Picture(Circle, Scale, Pictures, Text, Color, Translate), makeColor, azure, makeColorI )
import Graphics.Gloss.Interface.Pure.Game
    ( Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeyRight, KeyLeft, KeyDown, KeyUp, KeyEnter, KeyF5, KeyBackspace, KeyEsc, KeySpace),
      Event(EventKey) )
import Graphics.Gloss.Interface.Pure.Simulate ( Picture(Polygon) )
import System.Console.Terminfo (restoreDefaultColors, Color)
import Graphics.Gloss.Raster.Array


data Opcoes = Jogar
            | Mapas
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
          | Mapa Mapas 
          | Jogando Mapas --------- 
          | VenceuJogo Opcoes1 Mapas -------
          | VenceuUltimoJogo Opcoes2 ----------
          | Pausa Opcoes3 Mapas ------------
          | Controlador2 Opcoes4 Mapas ---------
          deriving (Eq)

data DataJogo = Direita
          | Esquerda
          | Porta1
          | Caixa1            
          | Bloco1
          | Vazio1
          | Do
          | D
          | E            
          | Cima
          | Enter
          | Space

type Imagens = [(DataJogo, Picture)]

type World = (Menu, Jogo, Imagens)

window :: Display
window = FullScreen

fr :: Int
fr = 60

--desenha o mundo
desenhaWorld :: World -> Picture
----- desenha venceu ultimo jogo
desenhaWorld (VenceuUltimoJogo Menu1, jogo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN ==",Translate (-300) 0 $ Color blue $ Text ">MENU<",Translate (-400) 0 $ Text "EXIT"] -------
desenhaWorld (VenceuUltimoJogo Sair1, jogo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN ==",Translate (-300) 0 $ Text "MENU",Translate (-400) 0 $ Color blue $ Text ">EXIT<"] --------
--- desenha venceu jogo
desenhaWorld (VenceuJogo Next l, jogo, imagens) = Pictures [Translate (-160) 100 $ Color red $ Text "== WIN ==",Translate 0 (-140) $ Color blue $ Text ">NEXT<",Translate 0 (-280) $ Text "EXIT"]--------- 
desenhaWorld (VenceuJogo Menu l, jogo, imagens) = Pictures [Translate (-160) 100 $ Color red $ Text "== WIN ==",Translate 0 (-140) $ Text "NEXT",Translate 0 (-280) $ Color blue $ Text ">EXIT<"] ----------
------ desenha pausa
desenhaWorld (Pausa Continuar l, jogo, imagens) = Pictures [Translate (-200) 0 $ Color orange $ Text ">RESUME<", Translate (-600) 0 $ Text "MENU"] ----------
desenhaWorld (Pausa Menu3 l, jogo, imagens) = Pictures [Translate (-200) 0 $  Text "RESUME", Translate (-600) 0 $ Color orange $ Text ">MENU<"] -------------
----- desenha constrolador2
desenhaWorld (Controlador2 Continuar1 l, jogo, imagens) = Pictures ([Color blue $ desenhaOpcao ">RESUME<",Translate 0 (-140) $ desenhaOpcao "NEW GAME",Translate 0 (-280) $ desenhaOpcao "MAPS", Translate 0 (-420) $ desenhaOpcao "EXIT"] ++ comandos (Controlador2 Continuar1 l, jogo, imagens)) ----------
desenhaWorld (Controlador2 Novo l, jogo, imagens) = Pictures ([desenhaOpcao "RESUME",Color blue $ Translate 0 (-140) $ desenhaOpcao ">NEW GAME<",Translate 0 (-280) $ desenhaOpcao "MAPS", Translate 0 (-420) $ desenhaOpcao "EXIT"] ++ comandos (Controlador2 Novo l, jogo, imagens) ) ------------
desenhaWorld (Controlador2 Mapas1 l, jogo, imagens) = Pictures ([desenhaOpcao "RESUME",Translate 0 (-140) $ desenhaOpcao "NEW GAME",Color blue $ Translate 0 (-280) $ desenhaOpcao ">MAPS<", Translate 0 (-420) $ desenhaOpcao "EXIT"] ++ comandos (Controlador2 Mapas1 l, jogo, imagens)) ---------
desenhaWorld (Controlador2 Sair2 l, jogo, imagens) = Pictures ([desenhaOpcao "RESUME",Translate 0 (-140) $ desenhaOpcao "NEW GAME",Translate 0 (-280) $ desenhaOpcao "MAPS",Color blue $ Translate 0 (-420) $ desenhaOpcao ">EXIT<"] ++ comandos (Controlador2 Sair2 l, jogo, imagens)) --------------
-----desenha lista mapas --------------
desenhaWorld (Mapa Level1 , jogo, imagens) = Pictures [Translate (-400) 200 $ Color blue $ desenhaOpcao ">Level 1", Translate (-400)50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4",Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"] ---------
desenhaWorld (Mapa Level2, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Color blue $ Translate (-400) 50 $ desenhaOpcao ">Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4",Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"] ----------
desenhaWorld (Mapa Level3, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Color blue $ Translate (-400) (-90) $ desenhaOpcao ">Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4",Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"] -----------
desenhaWorld (Mapa Level4, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Color blue $ Translate (-400) (-230) $ desenhaOpcao ">Level 4",Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"] -----------
desenhaWorld (Mapa Level5, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4",Color blue $ Translate (-400) (-370) $ desenhaOpcao ">Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"] -------------
desenhaWorld (Mapa Level6, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4", Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Color blue $ Translate (250) 200 $ desenhaOpcao ">Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"]
desenhaWorld (Mapa Level7, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4", Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Color blue $ Translate 250 50 $ desenhaOpcao ">Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"]
desenhaWorld (Mapa Level8, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4", Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Color blue $ Translate 250 (-90) $ desenhaOpcao ">Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"]
desenhaWorld (Mapa Level9, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4", Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (-400) (-510) $ desenhaOpcao "Menu",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Color blue $ Translate 250 (-230) $ desenhaOpcao ">Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"]
desenhaWorld (Mapa Level10, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4", Translate (-400) (-370) $ desenhaOpcao "Level 5",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ Color blue $desenhaOpcao ">Level 10<",Translate (-400) (-510) $ desenhaOpcao "Menu"]
desenhaWorld (Mapa Menu2, jogo, imagens) = Pictures [Translate (-400) 200 $ desenhaOpcao "Level 1", Translate (-400) 50 $ desenhaOpcao "Level 2", Translate (-400) (-90) $ desenhaOpcao "Level 3",Translate (-400) (-230) $ desenhaOpcao "Level 4", Translate (-400) (-370) $ desenhaOpcao "Level 5",Color blue $ Translate (-400) (-510) $ desenhaOpcao ">Menu<",Translate (250) 200 $ desenhaOpcao "Level 6", Translate 250 50 $ desenhaOpcao "Level 7", Translate 250 (-90) $ desenhaOpcao "Level 8",Translate 250 (-230) $ desenhaOpcao "Level 9",Translate 250 (-370) $ desenhaOpcao "Level 10"]
-------desenha constrolador inicial -------------
desenhaWorld (Controlador Jogar, jogo, imagens) = Pictures ([Color blue $ desenhaOpcao ">PLAY<",Translate 0 (-140) $ desenhaOpcao "MAPS", Translate 0 (-280) $ desenhaOpcao "EXIT"] ++ comandos (Controlador Jogar, jogo, imagens))----------
desenhaWorld (Controlador Mapas, jogo, imagens) = Pictures ([desenhaOpcao "PLAY",Color blue $ Translate 0 (-140) $ desenhaOpcao ">MAPS<" , Translate 0 (-280) $ desenhaOpcao "EXIT"] ++ comandos (Controlador Mapas, jogo, imagens))---------
desenhaWorld (Controlador Sair, jogo, imagens) = Pictures ([desenhaOpcao "PLAY",Translate 0 (-140) $ desenhaOpcao "MAPS", Color blue $ Translate 0 (-280) $ desenhaOpcao ">EXIT<"] ++ comandos (Controlador Sair, jogo, imagens)) -----------
------------- desenha menu jogando
desenhaWorld (Jogando l, jogo, imagens) ----
  | l == Level1 = Translate (90) (-250) $ Scale 1.4 1.4 (Pictures desenho) --
  | l == Level2 = Translate (40) (-150) $ Scale 1.3 1.3 (Pictures desenho) ---
  | l == Level3 = Translate (125) (-105) $ Scale 1.3 1.3 (Pictures desenho) --
  | l == Level4 = Translate (0) (15) $ Scale 1.1 1.1 (Pictures desenho) ---
  | l == Level5 = Translate (0) (15) $ Scale 1.1 1.1 (Pictures desenho) --
  | l == Level6 = Translate (35) (-20) $ Scale 1.2 1.2 (Pictures desenho) --
  | l == Level7 = Translate (-70) (-15) $ Scale 1.1 1.1 (Pictures desenho) --
  | l == Level8 = Translate (-145) (100) $ Scale 0.9 0.9  (Pictures desenho) --
  | l == Level9 = Translate (-140) (140) $ Scale 0.8 0.8 (Pictures desenho) ----
  | l == Level10 = Translate (-180) (140) $ Scale 0.8 0.8 (Pictures desenho) ----
  | otherwise = Pictures desenho
  where
      desenho = desenhoMapa ++ [desenhoJogador]
      desenhoMapa
        = (desenhaMapa
             comprimento altura (getMapa (Jogando l, jogo, imagens)) imagens)
      desenhoJogador
        = desenhaJogador (getJogador (Jogando l, jogo, imagens)) imagens


comandos :: World -> [Picture]
comandos (menu, jogo, imagens) = [Color white $ Translate (700) (0) $ Scale 0.2 0.2 $ Text "WALK RIGHT",Color white $ Translate 700 (-70) $ Scale 0.2 0.2 $ Text "WALK LEFT" ,Color white $ Translate 700 (-140) $ Scale 0.2 0.2 $ Text "CLIMB",Color white $ Translate 700 (-210) $ Scale 0.2 0.2 $ Text "PICK/DROP BOX",Color white $ Translate 700 (-280) $ Scale 0.2 0.2 $ Text "RESTART",Color white $ Translate 700 (-350) $ Scale 0.2 0.2 $ Text "STOP GAME",Translate 650 (10) $ Scale 1.5 1.5 $ snd (head (tail(tail (tail (tail (tail (tail (tail imagens)))))))),Translate 650 (-60) $ Scale 1.5 1.5 $ snd (head (tail(tail(tail (tail (tail (tail (tail (tail imagens))))))))),Translate 650 (-130) $ Scale 1.5 1.5 $ snd (head (tail (tail (tail (tail (tail (tail imagens))))))),Translate 650 (-200) $ Scale 1.5 1.5 $ snd (head (tail(tail(tail(tail (tail (tail (tail (tail (tail imagens)))))))))),Translate 650 (-270) $ Scale 1.5 1.5 $ snd (head (tail(tail(tail(tail(tail (tail (tail (tail (tail (tail imagens))))))))))),Translate 650 (-340) $ Scale 1.5 1.5 $ snd (head (tail(tail(tail(tail(tail(tail (tail (tail (tail (tail (tail imagens)))))))))))) ]

                                                                                                                                                                                                                                                                                                 --         let imagens = [(Porta1,porta),(Direita,blockdude_direita),(Esquerda,blockdude_esquerda ),(Caixa1,caixa),(Bloco1,bloco),(Vazio1,vazia),(Cima,up),(D,right),(E,left),(Do,down),(Space,space),(Enter,enter)]


desenhaLinha :: Float -> Float -> [Peca] -> Imagens -> [Picture]
desenhaLinha x y (h:t) imagens = peca : resto
  where peca = desenhaPeca x y h imagens
        resto = desenhaLinha (x+l) y t imagens
desenhaLinha _ _ _ _ = []

desenhaPeca :: Float -> Float -> Peca -> Imagens -> Picture
desenhaPeca x y peca imagens = Translate x y imagem
  where imagem = whatImg peca imagens

whatImg :: Peca  -> Imagens -> Picture
whatImg x l
 |x == Vazio = snd (head (tail (tail (tail (tail (tail l))))))
 |x == Bloco  = snd (head (tail (tail (tail (tail l)))))
 |x == Porta  = snd (head l)
 |x == Caixa = snd (head (tail (tail (tail l))))
 |otherwise = undefined


desenhaMapa :: Float -> Float -> Mapa  -> Imagens -> [Picture]
desenhaMapa x y (h:t) imagens = linha ++ resto
   where linha = desenhaLinha x y h imagens
         resto = desenhaMapa x (y-l) t imagens
desenhaMapa _ _ _ _ = []

desenhaJogador :: Jogador -> Imagens -> Picture
desenhaJogador (Jogador (x,y) d b) imagens
 |d == Oeste = Translate ((realPlayerX x)-64) (realPlayerY y) (snd (head (tail (tail imagens))))
 |otherwise = Translate  ((realPlayerX x)-64) (realPlayerY y) (snd (head (tail imagens)))

realPlayerX :: Int -> Float
realPlayerX = (+ comprimento).(* l).realToFrac.succ

realPlayerY :: Int -> Float
realPlayerY = (+ altura).(* (-l)).realToFrac

altura :: Float
altura = 400

comprimento :: Float
comprimento = (-670.0)

getMapa :: World -> Mapa
getMapa (_,Jogo m j,_) =  m

getJogador :: World -> Jogador
getJogador (_,Jogo m j,_) = j

-- desenha uma certa string no ecra
desenhaOpcao :: String -> Picture
desenhaOpcao option = Color white $ Translate (-160) 100 $ Text option ----------

-- atribui funções as teclas
event :: Event -> World -> World
-----controlador2
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Continuar1 l, jogo, imagens) = (Jogando l, jogo, imagens) -----------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Novo l, jogo, imagens) = (Jogando Level1, Jogo level1 player1 , imagens) ------------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Mapas1 l, jogo, imagens) = (Mapa Level1, jogo, imagens) ---------------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador2 Sair2 l, jogo, imagens) = undefined --------------------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Continuar1 l, jogo, imagens) = (Controlador2 Sair2 l, jogo, imagens) ---------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Sair2 l, jogo, imagens) = (Controlador2 Mapas1 l, jogo, imagens) -------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Mapas1 l, jogo, imagens) = (Controlador2 Novo l, jogo, imagens) ------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador2 Novo l, jogo, imagens) = (Controlador2 Continuar1 l, jogo, imagens) -----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Controlador2 Continuar1 l, jogo, imagens) = (Controlador2 Novo l, jogo, imagens) ---------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Novo l, jogo, imagens) = (Controlador2 Mapas1 l, jogo, imagens) --------------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Mapas1 l, jogo, imagens) = (Controlador2 Sair2 l, jogo, imagens)-----------------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador2 Sair2 l, jogo, imagens) = (Controlador2 Continuar1 l, jogo, imagens) ----------------
-------------pausa
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Menu3 l, jogo , imagens) = (Controlador2 Continuar1 l, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Continuar l, jogo, imagens) = (Jogando l, jogo, imagens) -------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa Menu3 l, jogo , imagens) = (Pausa Continuar l, jogo, imagens) ------------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Pausa Continuar l, jogo, imagens) = (Pausa Menu3 l, jogo , imagens) ----------
------------contrrolador inicial
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, Jogo l p, imagens) = (Jogando Level1 , Jogo level1 player1 , imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imagens) = (Controlador Sair, jogo, imagens) ----------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imagens) = (Controlador Mapas, jogo, imagens) ------------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Mapas, jogo, imagens) = (Controlador Sair, jogo, imagens) ------------
event (EventKey (SpecialKey KeyEnter ) Down _ _) (Controlador Mapas, jogo, imagens) = (Mapa Level1, jogo, imagens) ---------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Mapas, jogo, imagens) = (Controlador Jogar, jogo, imagens) ----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imagens) = (Controlador Mapas, jogo, imagens) -------------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imagens) = undefined  ----------
------------- venceu jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Next l, jogo, imagens) = (Jogando (nextMundo l), Jogo (nextMap l) (nextPlayer l), imagens) ---
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu Level5, jogo, imagens) = (Controlador Jogar, jogo, imagens) ------------
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (Controlador Jogar, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Next l, jogo, imagens) = (VenceuJogo Menu l, jogo, imagens)----------
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (VenceuJogo Next l, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (VenceuJogo Next l, jogo, imagens)----------
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Next l, jogo, imagens) = (VenceuJogo Menu l, jogo, imagens)----------
-----mapas
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level5, jogo, imagens) = (Mapa Menu2, jogo,  imagens) ----------
event (EventKey (SpecialKey KeyDown) Down _ _) (Mapa Level1, jogo, imagens) = (Mapa Level2, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level2, jogo, imagens) = (Mapa Level3, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level3, jogo, imagens) = (Mapa Level4, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level4, jogo, imagens) = (Mapa Level5, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Menu2, jogo, imagens) = (Mapa Menu2, jogo,  imagens)
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Level1, jogo, imagens) = (Mapa Level1, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level5, jogo, imagens) = (Mapa Level4, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level4, jogo, imagens) = (Mapa Level3, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level3, jogo, imagens) = (Mapa Level2, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level2, jogo, imagens) = (Mapa Level1, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Menu2, jogo, imagens) = (Mapa Level5, jogo,  imagens) ---------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level6, jogo, imagens) = (Mapa Level7, jogo,  imagens) ----------
event (EventKey (SpecialKey KeyDown) Down _ _) (Mapa Level7, jogo, imagens) = (Mapa Level8, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level8, jogo, imagens) = (Mapa Level9, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level9, jogo, imagens) = (Mapa Level10, jogo,  imagens)----------
event (EventKey (SpecialKey KeyDown ) Down _ _) (Mapa Level10, jogo, imagens) = (Mapa Level10, jogo,  imagens)----------
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Level6, jogo, imagens) = (Mapa Level6, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level7, jogo, imagens) = (Mapa Level6, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level8, jogo, imagens) = (Mapa Level7, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level9, jogo, imagens) = (Mapa Level8, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Mapa Level10, jogo, imagens) = (Mapa Level9, jogo, imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level6, jogo, imagens) = (Mapa Level6, jogo,  imagens) ----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level7, jogo, imagens) = (Mapa Level7, jogo,  imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level8, jogo, imagens) = (Mapa Level8, jogo,  imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level9, jogo, imagens) = (Mapa Level9, jogo,  imagens)----------
event (EventKey (SpecialKey KeyRight ) Down _ _) (Mapa Level10, jogo, imagens) = (Mapa Level10, jogo,  imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level1, jogo, imagens) = (Mapa Level6, jogo, imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level2, jogo, imagens) = (Mapa Level7, jogo, imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level3, jogo, imagens) = (Mapa Level8, jogo, imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level4, jogo, imagens) = (Mapa Level9, jogo, imagens)----------
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapa Level5, jogo, imagens) = (Mapa Level10, jogo, imagens)----------
event (EventKey (SpecialKey KeyLeft ) Down _ _) (Mapa Level6, jogo, imagens) = (Mapa Level1, jogo,  imagens) ----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level7, jogo, imagens) = (Mapa Level2, jogo,  imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level8, jogo, imagens) = (Mapa Level3, jogo,  imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level9, jogo, imagens) = (Mapa Level4, jogo,  imagens)----------
event (EventKey (SpecialKey KeyLeft ) Down _ _) (Mapa Level10, jogo, imagens) = (Mapa Level5, jogo,  imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level1, jogo, imagens) = (Mapa Level1, jogo, imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level2, jogo, imagens) = (Mapa Level2, jogo, imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level3, jogo, imagens) = (Mapa Level3, jogo, imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level4, jogo, imagens) = (Mapa Level4, jogo, imagens)----------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapa Level5, jogo, imagens) = (Mapa Level5, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Menu2, jogo, imagens) = (Controlador Jogar, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa l, jogo, imagens) = (Jogando l, Jogo (whatMap l) (whatPlayer l), imagens) ---------
---- menu jogando
event (EventKey (SpecialKey KeySpace ) Down _ _) (Jogando l, jogo, imagens) = (Pausa Menu3 l, jogo , imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo Trepar, imagens) ------
event (EventKey (SpecialKey KeyRight) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo AndarDireita , imagens) -------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo AndarEsquerda , imagens) -------
event (EventKey (SpecialKey KeyDown) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo InterageCaixa , imagens) -------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, Jogo (whatMap l) (whatPlayer l) , imagens)
-----venceu ultimo jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuUltimoJogo Menu1, jogo, imagens) = (Controlador Jogar, jogo, imagens) ----------
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuUltimoJogo Sair1, jogo, imagens) = undefined ---------------
event (EventKey (SpecialKey KeyUp ) Down _ _) (VenceuUltimoJogo Sair1, jogo, imagens) = (VenceuUltimoJogo Menu1, jogo, imagens) ---------------
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuUltimoJogo Menu1, jogo, imagens) = (VenceuUltimoJogo Sair1, jogo, imagens) ---------------
event _ w = w -------------

---------
coordPorta :: (Int,Int) ->  Jogo -> Bool
coordPorta (x1,y1) (Jogo ([]:ps) (Jogador (x2,y2) d b)) = coordPorta (0,y1+1) (Jogo ps (Jogador (x2,y2) d b))
coordPorta (x1,y1) (Jogo ((h:t):ps) (Jogador (x2,y2) d b))
 |h == Porta && x1 == x2 && y1 == y2 = True
 |otherwise = coordPorta (x1+1,y2) (Jogo (t:ps) (Jogador (x2,y2) d b))
coordPorta (x1,y1) (Jogo [] (Jogador (x2,y2) d b)) = False
--------
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
----------
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
----------

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

l :: Float
l = 64.0

updateMundo :: Float -> World -> World
updateMundo _ (Jogando l, Jogo m (Jogador (x,y) d b) , imagens) = if coordPorta (0,0) (Jogo m (Jogador (x,y) d b)) ----------
                                                                     then if l == Level10                    --------------
                                                                          then (VenceuUltimoJogo Menu1, Jogo m (Jogador (x,y) d b), imagens) --------
                                                                          else (VenceuJogo Next l, Jogo m (Jogador (x,y) d b), imagens) ----------
                                                                     else (Jogando l, Jogo m (Jogador (x,y) d b), imagens) --------
updateMundo _ w = w

cor :: Graphics.Gloss.Raster.Array.Color 
cor = makeColorI 77 77 77 0

vazia :: Picture 
vazia = Color cor $ Polygon [(0,0),(0,l),(l,0),(l,l)]

main :: IO ()
main = do
  blockdude_direita <- loadBMP "img/right1.bmp"                                                                                            
  blockdude_esquerda <- loadBMP "img/left1.bmp"
  porta <- loadBMP "img/porta.bmp"
  caixa <- loadBMP "img/caixa.bmp"
  bloco <- loadBMP "img/bloco.bmp"

  up <- loadBMP "img/up.bmp"
  down <- loadBMP "img/do.bmp"
  left <- loadBMP "img/e.bmp"
  right <- loadBMP "img/d.bmp"
  space <- loadBMP "img/space.bmp"
  enter <- loadBMP "img/enter.bmp"
  

  let imagens = [(Porta1,porta),(Direita,blockdude_direita),(Esquerda,blockdude_esquerda ),(Caixa1,caixa),(Bloco1,bloco),(Vazio1,vazia),(Cima,up),(D,right),(E,left),(Do,down),(Space,space),(Enter,enter)]

  let estadoInicial = (Controlador Jogar, Jogo level1 player1, imagens)

  play
     window  
     cor  
     fr      
     estadoInicial 
     desenhaWorld  
     event 
     updateMundo 







-----continuar o jogo que deixou a jogar
-----mudar o menu para imagens
-----constrolos quando joga
-----ajustar os mapas para ficar bonito
-----mudar a cor do background