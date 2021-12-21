{-# LANGUAGE NamedFieldPuns #-}
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
import LI12122 ( Jogo (Jogo), Jogador (Jogador), Peca (Porta) )
import Tarefa1_2021li1g097 ()
import Tarefa2_2021li1g097 ()
import Tarefa3_2021li1g097 ()
import Tarefa4_2021li1g097 ()
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
import Data.ByteString.Builder.Extra (Next)


data Opcoes = Jogar
            | Mapas 
            | Sair

data Opcoes1 = Next
             | Menu

data Mapa = Level1
           | Level2
           | Level3
           | Level4
           | Level5
           | Level6
           | Level7
           | Level8
           | Level9
           | Level10

data Menu = Controlador Opcoes
          | Mapas Mapas 
          | Jogando 
          | VenceuJogo Opcoes1

data BlockDude = Direita
               | Esquerda
               | CarregandoD
               | CarregandoE
    deriving Eq

data Mundo = Mundo String String

type Imagens = [(BlockDude, Picture)]

type World = (Menu, Jogo, Mapa, Imagens)

window :: Display
window = FullScreen

fr :: Int
fr = 30

--desenha os textos
draw :: World -> Picture
draw (VenceuJogo Next, jogo, mundo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN =="]
draw (VenceuJogo Menu, jogo, mundo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "== WIN =="]
draw (Controlador Jogar, jogo,mundo, imagens) = Pictures [Color blue $ drawOption "PLAY",Translate 0 (-140) $ drawOption "MAPS", Translate 0 (-280) $ drawOption "EXIT"]
draw (Controlador Mapas, jogo, mundo, imagens) = Pictures [drawOption "PLAY",Color blue $ Translate 0 (-140) $ drawOption "MAPS" , Translate 0 (-280) $ drawOption "EXIT"]
draw (Controlador Sair, jogo, mundo, imagens) = Pictures [drawOption "PLAY",Translate 0 (-140) $ drawOption "MAPS", Color blue $ Translate 0 (-280) $ drawOption "EXIT"]

-- desenha uma certa string no ecra
drawOption :: String -> Picture
drawOption option = Translate (-160) 100 $ Scale 1 1 $ Text option

-- atribui funções as teclas
event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, mapa, imagens) = (Jogando , jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, mundo, imagens) = (Controlador Sair, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, mundo, imagens) = (Controlador Mapas, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Mapas, jogo, mundo, imagens) = (Controlador Sair, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter ) Down _ _) (Controlador Mapas, jogo, mundo, imagens) = (Mapas Level1, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Mapas, jogo, mundo, imagens) = (Controlador Jogar, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, mundo, imagens) = (Controlador Mapas, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, mundo, imagens) = (Controlador Jogar, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Next, jogo, mundo, imagens) = (Jogando , jogo, nextMundo mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu, jogo, mundo, imagens) = (Controlador Jogar, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Next, jogo, mundo, imagens) = (VenceuJogo Menu, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Menu, jogo, mundo, imagens) = (VenceuJogo Next, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Menu, jogo, mundo, imagens) = (VenceuJogo Next, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Next, jogo, mundo, imagens) = (VenceuJogo Menu, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level10, jogo, mundo, imagens) = (Mapas Level10, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level1, jogo, mundo, imagens) = (Mapas Level2, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level2, jogo, mundo, imagens) = (Mapas Level3, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level3, jogo, mundo, imagens) = (Mapas Level4, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level4, jogo, mundo, imagens) = (Mapas Level5, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level6, jogo, mundo, imagens) = (Mapas Level7, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level7, jogo, mundo, imagens) = (Mapas Level8, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level8, jogo, mundo, imagens) = (Mapas Level9, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (Mapas Level9, jogo, mundo, imagens) = (Mapas Level10, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level1, jogo, mundo, imagens) = (Mapas Level1, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level10, jogo, mundo, imagens) = (Mapas Level9, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level9, jogo, mundo, imagens) = (Mapas Level8, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level8, jogo, mundo, imagens) = (Mapas Level7, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level7, jogo, mundo, imagens) = (Mapas Level6, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level6, jogo, mundo, imagens) = (Mapas Level5, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level5, jogo, mundo, imagens) = (Mapas Level4, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level4, jogo, mundo, imagens) = (Mapas Level3, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level3, jogo, mundo, imagens) = (Mapas Level2, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (Mapas Level2, jogo, mundo, imagens) = (Mapas Level1, jogo, mundo, imagens)

event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level1, jogo, mundo, imagens) = (Mapas Level1, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level10, jogo, mundo, imagens) = (Mapas Level9, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level9, jogo, mundo, imagens) = (Mapas Level8, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level8, jogo, mundo, imagens) = (Mapas Level7, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level7, jogo, mundo, imagens) = (Mapas Level6, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level6, jogo, mundo, imagens) = (Mapas Level5, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level5, jogo, mundo, imagens) = (Mapas Level4, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level4, jogo, mundo, imagens) = (Mapas Level3, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level3, jogo, mundo, imagens) = (Mapas Level2, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Mapas Level2, jogo, mundo, imagens) = (Mapas Level1, jogo, mundo, imagens)


event _ (Jogando, Jogo m (Jogador (x,y) d b), mundo, imagens) = if coordPorta (0,0) (Jogo m (Jogador (x,y) d b))
                                                                     then (VenceuJogo Next,Jogo m (Jogador (x,y) d b), mundo, imagens)
                                                                     else (Jogando, Jogo m (Jogador (x,y) d b), mundo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Sair, jogo, mundo, imagens) = undefined
event _ w = w

---------
coordPorta :: (Int,Int) ->  Jogo -> Bool
coordPorta (x1,y1) (Jogo ([]:ps) (Jogador (x2,y2) d b)) = coordPorta (0,y1+1) (Jogo ps (Jogador (x2,y2) d b))
coordPorta (x1,y1) (Jogo ((h:t):ps) (Jogador (x2,y2) d b))
 |h == Porta && x1 == x2 && y1 == y2 = True
 |otherwise = coordPorta (x1+1,y2) (Jogo (t:ps) (Jogador (x2,y2) d b))

--------
nextMundo :: Mundo -> Mundo 
nextMundo Mundo m p = 
 |m == Level1 = Mundo Level2 Player 
 |m == Level2 = Mundo Level3 p
 |m == Level3 = Mundo Level4 p
 |m == Level4 = Mundo Level5 p
 |m == Level5 = Mundo Level6 p
 |m == Level6 = Mundo Level7 p
 |m == Level7 = Mundo Level8 p
 |m == Level8 = Mundo Level9 p
 |m == Level9 = Mundo Level10 p




{-main :: IO ()
main = do
  blockdude_direita <- loadBMP "assets/blockdude_direita.bmp"
  blockdude_esquerda <- loadBMP "assets/blockdude_esquerda.bmp"
  blockdude_caixaE <- loadBMP "assets/blockdude_caixaE.bmp"
  blockdude_caixaD <- loadBMP "assets/blockdude_caixaD.bmp"
  caixa <- loadBMP "assets/caixa.bmp"
  bloco <- loadBMP "assets/bloco.bmp"
  
  let imagens = [(Direita, blockdude_direita), (Esquerda, blockdude_esquerda), (CarregandoE,blockdude_caixaE), (CarregandoD,blockdude_caixaD)]

  let jogando1 = (Controlador Jogando, Jogo level1 player1 , Mundo level1 player1, imagens)
  let next2 = (Controlador Jogando, Jogo level2 player2 , Mundo level2 player2, imagens)
  let next3 = (Controlador Jogando, Jogo level3 player3 , Mundo level3 player3, imagens)
  let next4 = (Controlador Jogando, Jogo level4 player4 , Mundo level4 player4, imagens)
  let next5 = (Controlador Jogando, Jogo level5 player5 , Mundo level5 player5, imagens)
  let next6 = (Controlador Jogando, Jogo level6 player6 , Mundo level6 player6, imagens)
  let next7 = (Controlador Jogando, Jogo level7 player7 , Mundo level7 player7, imagens)
  let next8 = (Controlador Jogando, Jogo level8 player8 , Mundo level8 player8, imagens)
  let next9 = (Controlador Jogando, Jogo level9 player9 , Mundo level9 player9, imagens)
  let next10 = (Controlador Jogando, Jogo level10 player10 , Mundo level10 player10, imagens)
  
  play window white fr loadCertoEstado draw event 
 
data HighScore = Integer

data GameState = Estado {
                imagens :: [Picture],
                jogosDisponiveis :: [Jogo],
                emJogo :: Bool,
                emMenu :: Bool,
                jogoTerminado :: Bool,
                highscore :: HighScore,
                jogoAtual :: Jogo
        }

imagemCaixa :: GameState -> Picture
imagemCaixa e = head (imagens e)

colocaEmJogoE :: GameState -> GameState
colocaEmJogoE e = e {emJogo = True, emMenu = False}


atualizarHighscore :: GameState -> GameState
atualizarHighscore e
 |e { jogoTerminado } = e {highscore = recebePontos e}
 |otherwise = e


main :: IO ()
main = do
  rawData <- readFile "assets/nivel"
  playplay
    window
    background
    fps
    estadoJogo
    (`render` [ bloco
              , caixa
              , esquerda
              , direita
              , porta
              ])
    movimentoTeclas
    atualizaJogo
-}


























