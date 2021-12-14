
{- |
Module      : Tarefa5_2021li1g097
Description : Aplicação Gráfica
Copyright   : Mário Cunha <a100649@alunos.uminho.pt>;
            : Diogo Araújo  <a100544@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

import Graphics.Gloss.Interface.Pure.Game ( Picture, play, Display (FullScreen), Color, white )
import Graphics.Gloss ( loadBMP )
import LI12122 ( Jogo )
import Tarefa1_2021li1g097 ()
import Tarefa2_2021li1g097 ()
import Tarefa3_2021li1g097 ()
import Tarefa4_2021li1g097 ()
import Graphics.Gloss.Interface.Pure.Simulate (Display)

  
type HighScore = Integer 

--usando Records
data Estado = Estado {
                imagens :: [Picture],
                jogosDisponiveis :: [Jogo],
                emJogo :: Bool,
                emMenu :: Bool,
                jogoTerminado :: Bool,
                highscore :: HighScore,
                jogoAtual :: Jogo
        }

imagemCaixa :: Estado -> Picture
imagemCaixa e = head (imagens e)

--colocaEmJogo :: EstadoBloated -> EstadoBloated
--colocaEmJogo (EB p j emJ emM jT h jA) = (EB p j True False jT h jA)

colocaEmJogoE :: Estado -> Estado
colocaEmJogoE e = e {emJogo = True, emMenu = False}

atualizarHighscore :: Estado -> Estado
atualizarHighscore e
 |jogoTerminado e = e {highscore = HighScore + recebePontos e}
 |otherwise = e

main :: IO ()
main = do
  bloco <- loadBMP "assets/bloco.bmp"
  caixa <- loadBMP "assets/caixa.bmp"
  esquerda <- loadBMP "assets/esquerda.bmp"
  direita <- loadBMP "assets/direita.bmp"
  porta <- loadBMP "assets/porta.bmp"
  rawData <- readFile "assets/level"
  play
    window
    background
    fps
    estado
    (`render` [ bloco
              , caixa
              , esquerda
              , direita
              , porta
              ])
    movimentoTeclas
    atualizaJogo


window :: Display
window = FullScreen

background :: Color
background = white

fps :: Int
fps = 60




























