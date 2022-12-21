module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game







--import System.Random (randomR)

import LI12223
import Tarefa1_2022li1g079
import Tarefa2_2022li1g079
import Tarefa3_2022li1g079
import Tarefa4_2022li1g079
import Tarefa5_2022li1g079


data Opcao = Jogar
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | VenceuJogo
          | PerdeuJogo

--data Piece = Arvore | Nenhum | Carro | Tronco
    --deriving Eq

type World = (Menu, Jogo, Imagem, Pont)

type Pont = Float

type Imagem = [Picture]

window :: Display
window = FullScreen --InWindow "Teste" (1000, 1000) (0,0)

fr :: Int
fr = 50

mapaInicial :: Mapa
mapaInicial = (Mapa 10 [(Relva, [n,n,a,a,n,a,n,n,a,n]),
                        (Rio 2, [n,n,t,t,t,n,n,t,n,t]),
                        (Relva, [n,a,a,n,a,n,a,n,a,a]),
                        (Relva, [a,a,n,n,a,n,a,a,n,n]),
                        (Rio 1, [n,n,t,t,t,n,t,t,n,n]),
                        (Estrada 2, [n,n,n,c,n,c,n,n,c,c]),
                        (Relva, [n,n,n,a,a,n,n,n,a,n]),
                        (Relva, [a,n,n,a,n,n,n,n,a,a]),
                        (Estrada (-2), [c,n,n,n,c,n,n,n,c,n]),
                        (Relva, [a,a,n,n,n,n,a,a,n,a])]) 
                where a = Arvore
                      n = Nenhum
                      t = Tronco
                      c = Carro         

                       
estadoInicial :: Imagem -> World
estadoInicial imagem = (Opcoes Jogar, Jogo (Jogador (-80,-410)) (mapaInicial), imagem, 0) 



desenhaEstado :: World -> Picture
desenhaEstado (PerdeuJogo, jogo, imagem, pont) = Translate (-50) 0 $ Color red $ scale 0.5 0.5 $ Text ("Score: " ++ show (round pont))
desenhaEstado (Opcoes Jogar, jogo, imagem, pont) = Pictures [Color red $ desenhaOp "Jogar", Translate (-45) (-200) $  desenhaOp "Fechar"]
desenhaEstado (Opcoes Sair, jogo, imagem, pont) = Pictures [desenhaOp "Jogar", Color red $ Translate (-45) (-200) $ desenhaOp "Fechar"]
desenhaEstado (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) = Pictures $ (desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) (-900) (-425)) ++ (desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) (-900) (-425)) ++ [Translate i j $ player]
   where 
     i = fromIntegral x
     j = fromIntegral y
     player = head imagem


desenhaMapa :: World -> Float -> Float -> [Picture]
desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l []),image,n) x1 y1 = [circle 1]
desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter,ob):t)),imagem,n) x1 y1 | ter == Relva = (Translate x1 y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+185) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+370) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+555) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+740) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+925) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+1110) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+1295) y1 $ (!!)imagem 3):
                                                                                                   (Translate (x1+1480) y1 $ (!!) imagem 3): 
                                                                                                   (Translate (x1+1665) y1 $ (!!) imagem 3):
                                                                                                   (Translate (x1+1850) y1 $ (!!) imagem 3):
                                                                                                   desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t),imagem,n) x1 (y1+90)
                                                                                  | otherwise = desenhaMapaAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, ob):t)),imagem,n) x1 y1 {-VAI TE FODER 21 ass MUSK-}

desenhaMapaAux :: World -> Float -> Float -> [Picture]
desenhaMapaAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, ob):t)),imagem,n) x1 y1 = (Translate x1 y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+185) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+370) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+555) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+740) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+925) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+1110) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+1295) y1 $ (!!)imagem 2):
                                                                                                (Translate (x1+1480) y1 $ (!!) imagem 2): 
                                                                                                (Translate (x1+1665) y1 $ (!!) imagem 2):
                                                                                                (Translate (x1+1850) y1 $ (!!) imagem 2):
                                                                                                desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t),imagem,n) x1 (y1+90)

desenhaMapaAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, ob):t)),imagem,n) x1 y1 = (Translate x1 y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+185) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+370) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+555) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+740) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+925) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+1110) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+1295) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+1480) y1 $ ((!!) imagem 1)): 
                                                                                            (Translate (x1+1665) y1 $ ((!!) imagem 1)):
                                                                                            (Translate (x1+1850) y1 $ ((!!) imagem 1)): desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t),imagem,n) x1 (y1+90)



desenhaOp opc = Translate (-100) 50 $ Text opc

desenhaObs :: World -> Float -> Float -> [Picture]
desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, []):t)), imagem, n) x1 y1 = [circle 1]

desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, (o:t1)):t)),imagem,n) x1 y1 | o == Tronco = (Translate x1 (y1+10) $ (!!) imagem 5): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, t1):t)),imagem,n) (x1+90) y1
                                                                                          | otherwise = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, t1):t)),imagem,n) (x1+90) y1
desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, (o:t1)):t)),imagem,n) x1 y1 | o == Carro && vel > 0 = (Translate x1 y1 $ (!!) imagem 6): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, t1):t)),imagem,n) (x1+90) y1
                                                                                              | o == Carro && vel < 0 = (Translate x1 y1 $ (!!) imagem 7): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, t1):t)),imagem,n) (x1+90) y1
                                                                                              | otherwise = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, t1):t)),imagem,n) (x1+90) y1
desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Relva, (o:t1)):t)),imagem,n) x1 y1 | o == Arvore = (Translate x1 (y1+10) $ (!!) imagem 4): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Relva, t1):t)),imagem,n) (x1+90) y1
                                                                                        | otherwise = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Relva, t1):t)),imagem,n) (x1+90) y1

desenhaObsAux:: World -> Float -> Float -> [Picture] 
desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l []), imagem, n) x1 y1 = [circle 1]
desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, o):t)), imagem, n) x1 y1 = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, o):t)), imagem, n) x1 y1 ++ desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t), imagem, n) x1 (y1+90)

novoEstado :: Key -> (Int,Int) -> Jogo
novoEstado key (x,y) =
  let p = (x + dx, y + dy)
      (dx,dy) = case key of
        (SpecialKey KeyUp) -> (0,90)
        (SpecialKey KeyDown) -> (0,-90)
        (SpecialKey KeyLeft) -> (-90,0)
        (SpecialKey KeyRight) -> (90,0)
  in Jogo (Jogador p) (mapaInicial)
  --(deslizaJogo 0 (Jogo (Jogador p) (mapaInicial)))

{-moveMapa :: Key -> Jogo -> Jogo
moveMapa 
-}
--do num <- randomR (0,9::Int) era suposto gerar um numero para a deslizaJogo noa 

event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, i, pont) = (ModoJogo, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, i, pont) = (Opcoes Sair, jogo, i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, i, pont) = (Opcoes Sair, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, i, pont) = (Opcoes Jogar, jogo,i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,i, pont) = (Opcoes Jogar, jogo,i, pont)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, i, pont) = error "Jogo Fechou"
--continuar a jogar depois de vencer
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo, i ,pont) = estadoInicial i
--identificar que acabou o jogo
event _ (ModoJogo, (Jogo(Jogador(x,y)) (mapaInicial)),i,pont) | x >= 700 = (PerdeuJogo,(Jogo(Jogador (700,y)) (mapaInicial)), i, pont)
                                                              | x <= -700 = (PerdeuJogo,(Jogo(Jogador (700,y)) (mapaInicial)), i, pont)
                                                              | y <= -600 = (PerdeuJogo,(Jogo(Jogador (x,-600)) (mapaInicial)), i, pont)
 
--Modo Jogo
event (EventKey key Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = (ModoJogo, novoEstado key (x,y), i, pont)
--Andar para tras com o tempo
event _ (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = (ModoJogo, (Jogo(Jogador (x,y)) (mapaInicial)),i, pont)
--Nao reagir caso não aconteçam os casos em cima
event _ x = x


pontu :: Float -> World -> World
pontu p (PerdeuJogo, j, i, pont) = (PerdeuJogo, j, i, pont)
pontu p (o,j,i,pont) = (o,j,i,pont+p)


-------CORES---------
corFundo :: Color
corFundo = white

corRelva :: Color
corRelva = makeColor 0 150 0 0.6

corEstrada :: Color
corEstrada = dim (greyN 0.3)

corRio :: Color
corRio =  dark $ corFundo
----------------------


main :: IO ()
main = do
  character <- loadBMP "character.bmp"
  agua      <- loadBMP "agua.bmp"
  estrada   <- loadBMP "estrada.bmp"
  relva     <- loadBMP "relva.bmp"
  arvore    <- loadBMP "3.bmp"
  tronco    <- loadBMP "tronco.bmp"
  carro1    <- loadBMP "car.bmp"
  carro3    <- loadBMP "CarroAmarelo.bmp"
  let imagem = [scale 3 3 character,
                scale 1 0.4 agua,
                scale 0.3 0.143 estrada,
                scale 0.2 0.170 relva,
                scale 3.5 3.5 arvore,
                scale 0.2 0.5 tronco,
                scale 0.08 0.15 carro1,
                scale 0.08 0.15 carro3]
  play window corFundo fr (estadoInicial imagem) desenhaEstado event pontu
























