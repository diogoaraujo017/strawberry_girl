
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
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
      player1,
      player2,
      player3,
      player4,
      player5 )
import LI12122 ( Jogo (Jogo), Jogador (Jogador), Peca (Porta, Bloco), Mapa, Movimento (AndarDireita, AndarEsquerda, Trepar, InterageCaixa) )
import Tarefa1_2021li1g097 ()
import Tarefa2_2021li1g097 ()
import Tarefa3_2021li1g097 ()
import Tarefa4_2021li1g097 (moveJogador)
import Graphics.Gloss
    ( blue,
      green,
      red,
      white,
      play,
      loadBMP,
      Display(InWindow, FullScreen),
      Picture(Circle, Scale, Pictures, Text, Color, Translate) )
import Graphics.Gloss.Interface.Pure.Game
    ( Key(SpecialKey),
      KeyState(Down),
      SpecialKey(KeyRight, KeyLeft, KeyDown, KeyUp, KeyEnter),
      Event(EventKey) )
import Data.Maybe ( fromJust )


import Graphics.Gloss.Interface.Pure.Simulate ( Picture(Polygon) )

import System.Console.Terminfo (restoreDefaultColors)
import Language.Haskell.TH (tupleDataName)
import GHC.Real (underflowError)


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

data Mapas = Level1
           | Level2
           | Level3
           | Level4
           | Level5
           | Menu2
          deriving (Eq)

data Menu = Controlador Opcoes
          | Mapa Mapas
          | Jogando Mapas
          | VenceuJogo Opcoes1 Mapas
          | VenceuUltimoJogo Opcoes2
          deriving (Eq)

data DataJogo = Direita
          | Esquerda
          | Porta1
          | Caixa
          | Bloco1
          | Vazio

type Imagens = [(DataJogo, Picture)]

type World = (Menu, Jogo, Imagens)

window :: Display
window = FullScreen

fr :: Int
fr = 60

--desenha o mundo
desenhaWorld :: World -> Picture
desenhaWorld (VenceuUltimoJogo Menu1, jogo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN =="]
desenhaWorld (VenceuUltimoJogo Sair1, jogo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN =="]
desenhaWorld(VenceuJogo Next l, jogo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN =="]
desenhaWorld(VenceuJogo Menu l, jogo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN =="]
desenhaWorld (Mapa Level1 , jogo, imagens) = Pictures [Translate (-70) 200 $ Color blue $ desenhaOpcao ">Level 1<", Translate (-70) 50 $ desenhaOpcao "Level 2", Translate (-70) (-90) $ desenhaOpcao "Level 3",Translate (-70) (-230) $ desenhaOpcao "Level 4",Translate (-70) (-370) $ desenhaOpcao "Level 5",Translate (-70) (-510) $ desenhaOpcao "Menu"] ---------
desenhaWorld (Mapa Level2, jogo, imagens) = Pictures [Translate (-70) 200 $ desenhaOpcao "Level 1", Color blue $ Translate (-70) 50 $ desenhaOpcao ">Level 2<", Translate (-70) (-90) $ desenhaOpcao "Level 3",Translate (-70) (-230) $ desenhaOpcao "Level 4",Translate (-70) (-370) $ desenhaOpcao "Level 5",Translate (-70) (-510) $ desenhaOpcao "Menu"] ----------
desenhaWorld (Mapa Level3, jogo, imagens) = Pictures [Translate (-70) 200 $ desenhaOpcao "Level 1", Translate (-70) 50 $ desenhaOpcao "Level 2", Color blue $ Translate (-70) (-90) $ desenhaOpcao ">Level 3<",Translate (-70) (-230) $ desenhaOpcao "Level 4",Translate (-70) (-370) $ desenhaOpcao "Level 5",Translate (-70) (-510) $ desenhaOpcao "Menu"] -----------
desenhaWorld (Mapa Level4, jogo, imagens) = Pictures [Translate (-70) 200 $ desenhaOpcao "Level 1", Translate (-70) 50 $ desenhaOpcao "Level 2", Translate (-70) (-90) $ desenhaOpcao "Level 3",Color blue $ Translate (-70) (-230) $ desenhaOpcao ">Level 4<",Translate (-70) (-370) $ desenhaOpcao "Level 5",Translate (-70) (-510) $ desenhaOpcao "Menu"] -----------
desenhaWorld (Mapa Level5, jogo, imagens) = Pictures [Translate (-70) 200 $ desenhaOpcao "Level 1", Translate (-70) 50 $ desenhaOpcao "Level 2", Translate (-70) (-90) $ desenhaOpcao "Level 3",Translate (-70) (-230) $ desenhaOpcao "Level 4",Color blue $ Translate (-70) (-370) $ desenhaOpcao ">Level 5<",Translate (-70) (-510) $ desenhaOpcao "Menu"] -------------
desenhaWorld (Mapa Menu2, jogo, imagens) = Pictures [Translate (-70) 200 $ desenhaOpcao "Level 1", Translate (-70) 50 $ desenhaOpcao "Level 2", Translate (-70) (-90) $ desenhaOpcao "Level 3",Translate (-70) (-230) $ desenhaOpcao "Level 4", Translate (-70) (-370) $ desenhaOpcao "Level 5",Color blue $ Translate (-70) (-510) $ desenhaOpcao ">Menu<"]
desenhaWorld (Controlador Jogar, jogo, imagens) = Pictures [Color blue $ desenhaOpcao ">PLAY<",Translate 0 (-140) $ desenhaOpcao "MAPS", Translate 0 (-280) $ desenhaOpcao "EXIT"] ----------
desenhaWorld (Controlador Mapas, jogo, imagens) = Pictures [desenhaOpcao "PLAY",Color blue $ Translate 0 (-140) $ desenhaOpcao ">MAPS<" , Translate 0 (-280) $ desenhaOpcao "EXIT"] ---------
desenhaWorld (Controlador Sair, jogo, imagens) = Pictures [desenhaOpcao "PLAY",Translate 0 (-140) $ desenhaOpcao "MAPS", Color blue $ Translate 0 (-280) $ desenhaOpcao ">EXIT<"] -----------
desenhaWorld (Jogando l, jogo, imagens) = Pictures (desenhaMapa comprimento altura (getMapaString (Jogando l, jogo, imagens)) imagens)




desenhaLinha :: Float -> Float -> String -> Imagens -> [Picture]
desenhaLinha x y (h:t) imagens = peca : resto
  where peca = desenhaPeca x y h imagens
        resto = desenhaLinha (x+l) y t imagens
desenhaLinha _ _ _ _ = []



desenhaPeca :: Float -> Float -> Char -> Imagens -> Picture
desenhaPeca  x y peca imagens = Translate x y imagem
  where imagem = whatImg peca imagens

whatImg :: Char -> Imagens -> Picture
whatImg x l
 |x == ' ' = snd (last l)
 |x == '>' = snd (head (tail l))
 |x == '<' = snd (head (tail (tail l)))
 |x == 'X' = snd (head (tail (tail (tail (tail l)))))
 |x == 'P' = snd (head l)
 |x == 'C' = snd (head (tail (tail (tail l))))
 |otherwise = undefined 


desenhaMapa :: Float -> Float -> String -> Imagens -> [Picture]
desenhaMapa x y s imagens = linha ++ resto
   where linha = desenhaLinha x y (tiraPrimeiraLinha s) imagens
         resto = desenhaMapa x (y-l) (removePrimeiraLinha s) imagens
desenhaMapa _ _ [] _ = []

tiraPrimeiraLinha :: String -> String
tiraPrimeiraLinha (x:xs)
 |x == ' ' = " " ++ tiraPrimeiraLinha xs
 |x == '>' = ">" ++ tiraPrimeiraLinha xs
 |x == '<' = ">" ++ tiraPrimeiraLinha xs
 |x == 'X' = "X" ++ tiraPrimeiraLinha xs
 |x == 'P' = "P" ++ tiraPrimeiraLinha xs
 |x == 'C' = "C" ++ tiraPrimeiraLinha xs
 |otherwise = []
tiraPrimeiraLinha _ = []

removePrimeiraLinha :: String -> String
removePrimeiraLinha (x:xs)
 |x == ' ' = removePrimeiraLinha xs
 |x == '>' = removePrimeiraLinha xs
 |x == '<' = removePrimeiraLinha xs
 |x == 'X' = removePrimeiraLinha xs
 |x == 'P' = removePrimeiraLinha xs
 |x == 'C' = removePrimeiraLinha xs
 |otherwise = xs
removePrimeiraLinha _ = []

altura :: Float
altura = 200

comprimento :: Float
comprimento = -200.0

getMapaString :: World -> String
getMapaString (_,Jogo m j,_) = show (Jogo m j)

-- desenha uma certa string no ecra
desenhaOpcao :: String -> Picture
desenhaOpcao option = Translate (-160) 100 $ Scale 1 1 $ Text option ----------

-- atribui funções as teclas
event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, Jogo l p, imagens) = (Jogando Level1 , Jogo l p , imagens) ------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, imagens) = (Controlador Sair, jogo, imagens) ----------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, imagens) = (Controlador Mapas, jogo, imagens) ------------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Mapas, jogo, imagens) = (Controlador Sair, jogo, imagens) ------------
event (EventKey (SpecialKey KeyEnter ) Down _ _) (Controlador Mapas, jogo, imagens) = (Mapa Level1, jogo, imagens) ---------------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Mapas, jogo, imagens) = (Controlador Jogar, jogo, imagens) ----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, imagens) = (Controlador Mapas, jogo, imagens) -------------
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, imagens) = (Controlador Jogar, jogo, imagens) ----------
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Next l, jogo, imagens) = (Jogando (nextMundo l), Jogo (nextMap l) (nextPlayer l), imagens) ---
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu Level5, jogo, imagens) = (Controlador Jogar, jogo, imagens) ------------
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (Controlador Jogar, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Next l, jogo, imagens) = (VenceuJogo Menu l, jogo, imagens)----------
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (VenceuJogo Next l, jogo, imagens)----------
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Menu l, jogo, imagens) = (VenceuJogo Next l, jogo, imagens)----------
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Next l, jogo, imagens) = (VenceuJogo Menu l, jogo, imagens)----------
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
event (EventKey (SpecialKey KeyUp ) Down _ _) (Mapa Menu2, jogo, imagens) = (Mapa Level5, jogo,  imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo AndarEsquerda, imagens) --------
event (EventKey (SpecialKey KeyRight) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo AndarDireita, imagens) ---------
event (EventKey (SpecialKey KeyDown) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo InterageCaixa , imagens) -----------
event (EventKey (SpecialKey KeyUp) Down _ _) (Jogando l, jogo, imagens) = (Jogando l, moveJogador jogo Trepar , imagens) -------------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Level1, jogo, imagens) = (Jogando Level1 , Jogo level1 player1 , imagens) ---------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Level5, jogo, imagens) = (Jogando Level5 , Jogo level5 player5 , imagens) ----------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Level4, jogo, imagens) = (Jogando Level4 , Jogo level4 player4 , imagens) --------- 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Level3, jogo, imagens) = (Jogando Level3 , Jogo level3 player3 , imagens) ----------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapa Level2, jogo, imagens) = (Jogando Level2 , Jogo level2 player2 , imagens) ---------
event (EventKey (SpecialKey KeyEnter ) Down _ _) (Mapa Menu2, jogo, imagens) = (Controlador Jogar , jogo,  imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Jogando l, jogo, imagens) =  (Jogando l, moveJogador jogo Trepar, imagens) ------
event (EventKey (SpecialKey KeyRight) Down _ _) (Jogando l, jogo, imagens) =  (Jogando l, moveJogador jogo AndarDireita , imagens) -------
event (EventKey (SpecialKey KeyLeft) Down _ _) (Jogando l, jogo, imagens) =  (Jogando l, moveJogador jogo AndarEsquerda , imagens) -------
event (EventKey (SpecialKey KeyDown) Down _ _) (Jogando l, jogo, imagens) =  (Jogando l, moveJogador jogo InterageCaixa , imagens) -------
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, imagens) = undefined ----------
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
--------
nextMundo :: Mapas -> Mapas
nextMundo l
 |l == Level1 = Level2
 |l == Level2 = Level3
 |l == Level3 = Level4
 |l == Level4 = Level5
nextMundo _ = undefined
----------
nextMap :: Mapas -> Mapa
nextMap l
 | l == Level1 = level2
 | l == Level2 = level3
 | l == Level3 = level4
 | l == Level4 = level5
----------
nextPlayer :: Mapas -> Jogador
nextPlayer l
 | l == Level1 = player2
 | l == Level2 = player3
 | l == Level3 = player4
 | l == Level4 = player5

l :: Float
l = 64.0


updateMundo :: Float -> World -> World
updateMundo _ (Jogando l, Jogo m (Jogador (x,y) d b) , imagens) = if coordPorta (0,0) (Jogo m (Jogador (x,y) d b)) ----------
                                                                     then if l == Level5                    --------------
                                                                          then (VenceuUltimoJogo Menu1, Jogo m (Jogador (x,y) d b), imagens) --------
                                                                          else (VenceuJogo Next l, Jogo m (Jogador (x,y) d b), imagens) ----------
                                                                     else (Jogando l, Jogo m (Jogador (x,y) d b), imagens) --------
updateMundo _ w = w


main :: IO ()
main = do
  blockdude_direita <- loadBMP "img/right1.bmp"
  blockdude_esquerda <- loadBMP "img/left1.bmp"
  porta <- loadBMP "img/door.bmp"
  caixa <- loadBMP "img/food.bmp"
  bloco <- loadBMP "img/tile.bmp"
  vazia <- loadBMP "img/white.bmp"

  let imagens = [(Porta1,porta),(Direita,blockdude_direita),(Esquerda,blockdude_esquerda ),(Caixa,caixa),(Bloco1,bloco),(Vazio,vazia)]

  let estadoInicial = (Controlador Jogar, Jogo level1 player1, imagens)

  play
     window  ---
     white   --- 
     fr      ---
     estadoInicial ----
     desenhaWorld   ------
     event -------
     updateMundo --------
--------------------------