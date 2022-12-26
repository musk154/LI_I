module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game







--import System.Random (randomRIO)

import LI12223
import Tarefa1_2022li1g079
import Tarefa2_2022li1g079
import Tarefa3_2022li1g079
import Tarefa4_2022li1g079
import Tarefa5_2022li1g079


data Opcao = Jogar
            | Sair
            | Skins

data Menu = Opcoes Opcao
          | ModoJogo 
          | ModoJogo1
          | PerdeuJogo
          | ModoSkins Botao 

data Botao = Skin1
            | Skin2
            | Skin3

type World = (Menu, Jogo, Imagem, Pont)

type Pont = Float

type Imagem = [Picture]

window :: Display
window = FullScreen --InWindow "Teste" (1000, 1000) (0,0)

fr :: Int
fr = 1

mapaInicial :: Mapa
mapaInicial = (Mapa 21 [(Relva, [a,a,n,n,n,n,a,a,n,a,n,n,n,n,a,n,n,n,n,a,n]),
                        (Relva, [n,n,a,n,n,n,n,a,n,n,a,a,n,n,n,a,a,a,n,n,a]),
                        (Estrada (-1), [c,n,n,n,c,n,n,n,c,n,c,n,n,n,n,n,n,c,c,n,n]),
                        (Relva, [a,n,n,a,n,n,n,n,a,a,n,n,n,a,a,n,n,n,a,a,n]),
                        (Relva, [n,n,n,a,a,n,n,n,a,n,n,a,a,n,a,n,n,n,n,a,n]),
                        (Estrada 1, [n,n,n,c,n,c,n,n,c,c,n,n,c,c,n,n,n,c,n,n,n]),
                        (Rio 1, [n,n,t,t,t,n,t,t,n,n,n,n,t,t,n,n,n,n,n,t,t]),
                        (Relva, [a,a,n,n,a,n,n,a,n,n,n,a,a,n,n,a,n,n,n,n,n]),
                        (Relva, [n,n,n,n,a,n,a,n,n,a,n,n,n,n,a,n,n,n,n,a,n]),
                        (Rio 1, [n,n,t,t,t,n,n,t,n,t,n,n,t,t,t,n,n,n,n,n,t]),
                        (Estrada (-1), [c,n,n,n,n,n,n,n,c,n,c,n,n,n,n,c,c,c,n,n,n]),
                        (Relva, [n,n,a,a,n,a,n,n,a,n,n,n,a,a,n,n,n,n,a,n,n])]) 
                where a = Arvore
                      n = Nenhum
                      t = Tronco
                      c = Carro         

estadoInicial :: Imagem -> World
estadoInicial imagem = (Opcoes Jogar, Jogo (Jogador (0,-475)) (mapaInicial), imagem, 0) 



desenhaEstado :: World -> Picture
desenhaEstado (PerdeuJogo, jogo, imagem, pont) = Pictures $ [(Translate 0 0 $ (!!) imagem 11)] ++ [Translate (-10) (-200) $ (Text (show (round pont)))]
desenhaEstado (Opcoes Jogar, jogo, imagem, pont) = Pictures $ [(Translate 0 0 $ (!!) imagem 8)] ++ [(Translate 0 10 $ (!!) imagem 12)] ++ [(Translate 0 0 $ (!!) imagem 9)] ++ [(Translate 0 (-200) $ (!!) imagem 13)] ++ [(Translate 0 (-400) $ (!!) imagem 10)]
desenhaEstado (Opcoes Sair, jogo, imagem, pont) = Pictures $ [(Translate 0 0 $ (!!) imagem 8)] ++ [(Translate 0 (-400) $ (!!) imagem 12)] ++ [(Translate 0 0 $ (!!) imagem 9)] ++ [(Translate 0 (-200) $ (!!) imagem 13)] ++ [(Translate 0 (-400) $ (!!) imagem 10)]
desenhaEstado (Opcoes Skins, jogo,imagem, pont) = Pictures $ [(Translate 0 0 $ (!!) imagem 8)] ++ [(Translate 0 (-190) $ (!!) imagem 12)] ++ [(Translate 0 0 $ (!!) imagem 9)] ++ [(Translate 0 (-200) $ (!!) imagem 13)] ++ [(Translate 0 (-400) $ (!!) imagem 10)]
desenhaEstado (ModoSkins Skin1, jogo, imagem, pont) = Pictures $ [(Translate 0 0 $ (!!) imagem 8)] ++ [(Translate 0 (-190) $ (!!) imagem 14)]
desenhaEstado (ModoSkins Skin2, jogo, imagem, pont) = Pictures $ [(Translate 0 0 $ (!!) imagem 8)] ++ [(Translate 0 (-190) $ (!!) imagem 15)] ++ [(Translate 0 0 $ (!!) imagem 16)]
desenhaEstado (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) = Pictures $ (desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) (-900) (515)) ++ (desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) (-900) (515)) ++ [Translate i j $ player]
 where 
     i = fromIntegral x
     j = fromIntegral y
     player = head imagem

desenhaEstado (ModoJogo1, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) = Pictures $ (desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) (-900) (515)) ++ (desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial), imagem, n) (-900) (515)) ++ [Translate i j $ (!!) imagem 17]
   where 
     i = fromIntegral x
     j = fromIntegral y
     

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
                                                                                                   desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t),imagem,n) x1 (y1-90)
                                                                                  | otherwise = desenhaMapaAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, ob):t)),imagem,n) x1 y1 

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
                                                                                                desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t),imagem,n) x1 (y1-90)

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
                                                                                            (Translate (x1+1850) y1 $ ((!!) imagem 1)): desenhaMapa (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t),imagem,n) x1 (y1-90)



desenhaOp opc = Translate (-150) 50 $ Text opc

desenhaObs :: World -> Float -> Float -> [Picture]
desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, []):t)), imagem, n) x1 y1 = [circle 1] -- caso de paragem porque um circulo de raio 1 não se vê

desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, (o:t1)):t)),imagem,n) x1 y1 | o == Tronco = (Translate x1 (y1+10) $ (!!) imagem 5): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, t1):t)),imagem,n) (x1+90) y1
                                                                                          | otherwise = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Rio vel, t1):t)),imagem,n) (x1+90) y1

desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, (o:t1)):t)),imagem,n) x1 y1 | o == Carro && vel > 0 = (Translate x1 y1 $ (!!) imagem 6): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, t1):t)),imagem,n) (x1+90) y1
                                                                                              | o == Carro && vel < 0 = (Translate x1 y1 $ (!!) imagem 7): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, t1):t)),imagem,n) (x1+90) y1
                                                                                              | otherwise = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Estrada vel, t1):t)),imagem,n) (x1+90) y1

desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Relva, (o:t1)):t)),imagem,n) x1 y1 | o == Arvore = (Translate x1 (y1+10) $ (!!) imagem 4): desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Relva, t1):t)),imagem,n) (x1+90) y1
                                                                                        | otherwise = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((Relva, t1):t)),imagem,n) (x1+90) y1

desenhaObsAux:: World -> Float -> Float -> [Picture] 
desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l []), imagem, n) x1 y1 = [circle 1]
desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, o):t)), imagem, n) x1 y1 = desenhaObs (ModoJogo, Jogo (Jogador (x,y)) (Mapa l ((ter, o):t)), imagem, n) x1 y1 ++ desenhaObsAux (ModoJogo, Jogo (Jogador (x,y)) (Mapa l t), imagem, n) x1 (y1-90)



event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, i, pont) = (ModoJogo, jogo, i, pont)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Skins, jogo, i, pont) = (ModoSkins Skin1, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, i, pont) = (Opcoes Sair, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, i, pont) = (Opcoes Skins, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Skins, jogo, i, pont) = (Opcoes Jogar, jogo, i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, i, pont) = (Opcoes Skins, jogo, i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Skins, jogo, i, pont) = (Opcoes Sair, jogo, i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,i, pont) = (Opcoes Jogar, jogo,i, pont)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, i, pont) = error "Jogo Fechou"


--continuar a jogar depois de vencer
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo, i ,pont) = estadoInicial i
--menu skins
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoSkins Skin1, jogo, i, pont) = (ModoSkins Skin2, jogo, i, pont)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoSkins Skin1, jogo, i, pont) = (ModoSkins Skin2, jogo, i, pont)  
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoSkins Skin2, jogo, i, pont) = (ModoSkins Skin1, jogo, i, pont)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoSkins Skin2, jogo, i, pont) = (ModoSkins Skin1, jogo, i, pont)
event (EventKey (SpecialKey KeyEnter) Down _ _) (ModoSkins Skin1, jogo, i, pont) = (ModoJogo, jogo, i, pont)
event (EventKey (SpecialKey KeyEnter) Down _ _) (ModoSkins Skin2, jogo, i, pont) = (ModoJogo1, jogo, i, pont)

--modo Jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Cima)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Cima)) mapaInicial), i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Baixo)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Baixo)) mapaInicial), i, pont)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Direita)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Direita)) mapaInicial), i, pont)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Esquerda)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Esquerda)) mapaInicial), i, pont)

event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo1, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Cima)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo1, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Cima)) mapaInicial), i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo1, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Baixo)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo1, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Baixo)) mapaInicial), i, pont)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo1, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Direita)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo1, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Direita)) mapaInicial), i, pont)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo1, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = if jogoTerminou (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Esquerda)) mapaInicial) == True then (PerdeuJogo,(Jogo (Jogador (x,y)) (mapaInicial)),i,pont) else (ModoJogo1, (Jogo (player(Jogador (x,y)) (mapaInicial) (Move Esquerda)) mapaInicial), i, pont)

--Nao reagir caso não aconteçam os casos em cima
event _ x = x


pontu :: Float -> World -> World
pontu p (ModoJogo, j, i, pont) = if jogoTerminou (animaJogo j (Parado)) == True then (PerdeuJogo,j,i,pont) else (ModoJogo, animaJogo j (Parado), i, pont+p)
pontu p (ModoJogo1, j, i, pont) = if jogoTerminou (animaJogo j (Parado)) == True then (PerdeuJogo,j,i,pont) else (ModoJogo1, animaJogo j (Parado), i, pont+p)
--pontu p (ModoJogo, i, j, pont) = (ModoJogo, deslizaJogo num j (Parado), i, pont)
pontu p (PerdeuJogo, j, i, pont) = (PerdeuJogo, j, i, pont)
pontu p (o,j,i,pont) = (o,j,i,pont+p)
--função que deteta se o player saiu do mapa estando em cima do tronco vai ter de ser chamada aqui

{-geraNum :: [num] -> IO num 
geraNum  num = do {r <- randomRIO (1, length num)
                ; return $ num !! (r - 1)
                }
-}
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
  character   <- loadBMP "character.bmp"
  agua        <- loadBMP "agua.bmp"
  estrada     <- loadBMP "estrada.bmp"
  relva       <- loadBMP "relva_1_.bmp"
  arvore      <- loadBMP "arvore1.bmp"
  tronco      <- loadBMP "tronco.bmp"
  carro1      <- loadBMP "car.bmp"
  carro3      <- loadBMP "CarroAmarelo.bmp"
  fundo       <- loadBMP "pixel-art.bmp"
  jogar       <- loadBMP "jogar.bmp"
  sair        <- loadBMP "sair.bmp"
  score       <- loadBMP "score.bmp"
  papel       <- loadBMP "papel.bmp"
  personagens <- loadBMP "Personagens.bmp"
  p1          <- loadBMP "p1.bmp"
  p2          <- loadBMP "p2.bmp"
  pinkmonster <- loadBMP "Pink_Monster.bmp"
  pinkmonster1<- loadBMP "Pink_Monster_Walk.bmp"
  let imagem = [scale 3 3 character,
                scale 1 0.4 agua,
                scale 0.3 0.143 estrada,
                scale 0.2 0.170 relva,
                scale 0.25 0.2 arvore,
                scale 0.2 0.5 tronco,
                scale 0.08 0.15 carro1,
                scale 0.08 0.15 carro3,
                scale 1.9 1.75 fundo,
                scale 1 1 jogar,
                scale 1 1 sair,
                scale 1 1 score,
                scale 1.1 0.6 papel,
                scale 1.3 1.3 personagens,
                scale 1 1 p1,
                scale 1 1 p2,
                scale 7 7 pinkmonster,
                scale 3 3 pinkmonster1]
  play window corFundo fr (estadoInicial imagem) desenhaEstado event pontu






















