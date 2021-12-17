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

data Menu = Controlador Opcoes
          | Jogando 
          | VenceuJogo Opcoes1

data BlockDude = Direita
               | Esquerda
               | CarregandoD
               | CarregandoE
    deriving Eq

data Mundo = Mundo Picture Jogador

type Imagens = [(BlockDude, Picture)]

type World = (Menu, Jogo, Mundo, Imagens)

window :: Display
window = FullScreen

fr :: Int
fr = 30

--desenha os textos
draw :: World -> Picture
draw (VenceuJogo Next, jogo, mundo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "Ganhou :)"]
draw (VenceuJogo Menu, jogo, mundo, imagens) = Pictures [Translate (-200) 0 $ Color red $ Text "Ganhou :)"]
draw (Controlador Jogar, jogo,mundo, imagens) = Pictures [Color blue $ drawOption "Jogar",Translate 0 (-140) $ drawOption "Mapas", Translate 0 (-280) $ drawOption "Sair"]
draw (Controlador Mapas, jogo, mundo, imagens) = Pictures [drawOption "Jogar",Color blue $ Translate 0 (-140) $ drawOption "Mapas" , Translate 0 (-280) $ drawOption "Sair"]
draw (Controlador Sair, jogo, mundo, imagens) = Pictures [drawOption "Jogar",Translate 0 (-140) $ drawOption "Mapas", Color blue $ Translate 0 (-280) $ drawOption "Sair"]

-- desenha uma certa string no ecra
drawOption :: String -> Picture
drawOption option = Translate (-160) 100 $ Scale 1 1 $ Text option

-- atribui funções as teclas
event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar, jogo, mundo, imagens) = (Jogando , jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Jogar, jogo, mundo, imagens) = (Controlador Sair, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Jogar, jogo, mundo, imagens) = (Controlador Mapas, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Mapas, jogo, mundo, imagens) = (Controlador Sair, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Mapas, jogo, mundo, imagens) = (Controlador Jogar, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (Controlador Sair, jogo, mundo, imagens) = (Controlador Mapas, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (Controlador Sair, jogo, mundo, imagens) = (Controlador Jogar, jogo, mundo, imagens)


event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Next, jogo, mundo, imagens) = 
event (EventKey (SpecialKey KeyEnter) Down _ _) (VenceuJogo Menu, jogo, mundo, imagens) = (Controlador Jogar, jogo, mundo, imagens)


event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Next, jogo, mundo, imagens) = (VenceuJogo Menu, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Menu, jogo, mundo, imagens) = (VenceuJogo Next, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (VenceuJogo Menu, jogo, mundo, imagens) = (VenceuJogo Next, jogo, mundo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (VenceuJogo Next, jogo, mundo, imagens) = (VenceuJogo Menu, jogo, mundo, imagens)
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

{-main :: IO ()
main = do
  blockdude_direita <- loadBMP "assets/blockdude_direita.bmp"
  blockdude_esquerda <- loadBMP "assets/blockdude_esquerda.bmp"
  blockdude_caixaE <- loadBMP "assets/blockdude_caixaE.bmp"
  blockdude_caixaD <- loadBMP "assets/blockdude_caixaD.bmp"
  caixa <- loadBMP "assets/caixa.bmp"
  bloco <- loadBMP "assets/bloco.bmp"
  
  let imagens = [(Direita, blockdude_direita), (Esquerda, blockdude_esquerda), (CarregandoE,blockdude_caixaE), (CarregandoD,blockdude_caixaD)]

  let estado1 = (Controlador Jogar, Mundo level1 player1, imagens)
  let estado2 = (Controlador Jogar, Mundo level2 player2, imagens)
  let estado3 = (Controlador Jogar, Mundo level3 player3, imagens)
  let estado4 = (Controlador Jogar, Mundo level4 player4, imagens)
  let estado5 = (Controlador Jogar, Mundo level5 player5, imagens)
  let estado6 = (Controlador Jogar, Mundo level6 player6, imagens)
  let estado7 = (Controlador Jogar, Mundo level7 player7, imagens)
  let estado8 = (Controlador Jogar, Mundo level8 player8, imagens)
  let estado9 = (Controlador Jogar, Mundo level9 player9, imagens)
  let estado10 = (Controlador Jogar, Mundo level10 player10, imagens)

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


























