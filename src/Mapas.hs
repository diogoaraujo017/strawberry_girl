module Mapas where
import LI12122 (Mapa, Jogador (Jogador), Peca(Vazio, Bloco, Porta), Peca(Caixa), Direcao (Oeste))



player1 :: Jogador
player1 = (Jogador (6, 0) Oeste False) 

player2 :: Jogador
player2 = undefined 

player3 :: Jogador
player3 = undefined

player4 :: Jogador 
player4 = undefined

player5 :: Jogador 
player5 = undefined 





level1 :: Mapa 
level1 = [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
           [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
           [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
           [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
           [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
     ]

level2 :: Mapa 
level2 = undefined

level3 :: Mapa  
level3 = undefined

level4 :: Mapa  
level4 = undefined

level5 :: Mapa  
level5 = undefined 


