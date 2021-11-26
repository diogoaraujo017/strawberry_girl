module Tarefa3_2021li1g097_Spec where

import Test.HUnit
import Tarefa3_2021li1g097
import Fixtures

testsT3 =
  test
    [ "Tarefa 3 - Teste Imprime Jogo m1e1" ~: "      <\n      X\n      X\nP   C X\nXXXXXXX" ~=?  show m1e1
    , "Tarefa 3 - Teste Imprime Jogo m1e2" ~: "       \n      X\n      X\nP < C X\nXXXXXXX" ~=?  show m1e2
    , "Tarefa 3 - Teste Imprime Jogo m2e1" ~: "     XXX    XXXXXXXXX \n XXXX   XXXX         X\nX                    X\nX                    X\nX                    X\nX     X              X\nX     X              X\nX     XCCCC          X\nXP   XXXXXXX<        X\nXX XXX     XX X     CX\n X X        X XX   CCX\n X X        X XX  CCCX\n XXX        X XXXXXXXX\n            XXX       " ~=?  show m2e1
    , "Tarefa 3 - Teste Imprime Jogo m2e2" ~: "     XXX    XXXXXXXXX \n XXXX   XXXX         X\nX                    X\nX                    X\nX                    X\nX     X              X\nX     X              X\nX     XCCCC          X\nXP   XXXXXXX        >X\nXX XXX     XX X     CX\n X X        X XX   CCX\n X X        X XX  CCCX\n XXX        X XXXXXXXX\n            XXX       " ~=?  show m2e2
    , "Tarefa 3 - Teste Imprime Jogo m3e1" ~: " X                 \n X   XXXXXXXXXXXXX \nX X X             X\nX  X              X\nX                CX\nX               CCX\nX XXX    <   XC XX \nX X X    X  XXXXX  \nX X XCC XX  X      \nXPX XXXXXX XX      \nXXX XX   XXX       " ~=?  show m3e1
    , "Tarefa 3 - Teste Imprime Jogo m3e2" ~: " X                 \n X   XXXXXXXXXXXXX \nX X X             X\nX  X             >X\nX                CX\nX               CCX\nX XXX        XC XX \nX X X    X  XXXXX  \nX X XCC XX  X      \nXPX XXXXXX XX      \nXXX XX   XXX       " ~=?  show m3e2
    ]


