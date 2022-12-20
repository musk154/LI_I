module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Game


import LI12223
import Tarefa1_2022li1g079
import Tarefa2_2022li1g079
import Tarefa3_2022li1g079
import Tarefa4_2022li1g079


data Opcao = Jogar
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | VenceuJogo
          | PerdeuJogo

type World = (Menu, Jogo, Imagem, Pont)

type Pont = Float

type Imagem = [Picture]

window :: Display
window = InWindow "Teste" (1000, 1000) (0,0)

fr :: Int
fr = 50

mapaInicial :: Mapa
mapaInicial = (Mapa 10 [(Rio 2, [n,n,t,t,t,n,n,t,n,t]),
                        (Relva, [n,n,a,a,n,a,n,n,a,n]),
                        (Relva, [n,a,a,n,a,n,a,n,a,a]),
                        (Relva, [a,a,n,n,a,n,a,a,n,n]),
                        (Rio 1, [n,n,t,t,t,n,t,t,n,n]),
                        (Estrada 2, [n,n,n,c,n,c,n,n,c,c]),
                        (Relva, [n,n,n,t,t,n,n,n,t,n]),
                        (Relva, [t,n,n,t,n,n,n,n,t,t]),
                        (Estrada (-2), [c,n,n,n,c,n,n,n,c,n]),
                        (Relva, [t,t,n,n,n,n,t,t,n,t])])
                where a = Arvore
                      n = Nenhum
                      t = Tronco
                      c = Carro         



	                     
estadoInicial :: Imagem -> World
estadoInicial imagem = (Opcoes Jogar, Jogo (Jogador (0,-437)) (mapaInicial), imagem, 0)


desenhaEstado :: World -> Picture
desenhaEstado (PerdeuJogo, jogo, imagem, pont) = Translate (-50) 0 $ Color red $ scale 0.5 0.5 $ Text ("Score: " ++ show (round pont))
desenhaEstado (Opcoes Jogar, jogo, imagem, pont) = Pictures [Color red $ desenhaOp "Jogar", Translate 0 (-200) $  desenhaOp "Fechar"]
desenhaEstado (Opcoes Sair, jogo, imagem, pont) = Pictures [desenhaOp "Jogar", Color red $ Translate 0 (-200) $ desenhaOp "Fechar"]
desenhaEstado (ModoJogo, Jogo (Jogador (x,y)) (mapaInicial),imagem,n) = Pictures $ (desenhaMapa mapaInicial (-900) (-425)) ++ [Translate i j $ player]
   where 
   	 i = fromIntegral x
   	 j = fromIntegral y
   	 player = head imagem


desenhaMapa :: Mapa -> Float -> Float -> [Picture]
desenhaMapa (Mapa l []) x y = [circle 1]
desenhaMapa (Mapa l ((ter,ob):t)) x y | ter == Relva = (Translate x y $ (color green $ rectangleSolid 9999 100)) : desenhaMapa (Mapa l t) x (y + 100)
                                      | otherwise = desenhaMapaAux (Mapa l ((ter, ob):t)) x y

desenhaMapaAux :: Mapa -> Float -> Float -> [Picture]
desenhaMapaAux (Mapa l ((Estrada vel, ob):t)) x y = (Translate x y $ (color (greyN 0.4) $ rectangleSolid 9999 100)) : desenhaMapa (Mapa l t) x (y+100)
desenhaMapaAux (Mapa l ((Rio vel, ob):t)) x y = (Translate x y $ (color blue $ rectangleSolid 9999 100)) : desenhaMapa (Mapa l t) x (y+100)



desenhaOp opc = Translate 0 50 $ Text opc


novoEstado :: Key -> (Int,Int) -> Jogo
novoEstado key (x,y) =
	let p = (x + dx, y + dy)
	    (dx,dy) = case key of
	     	(SpecialKey KeyUp) -> (0,100)
	     	(SpecialKey KeyDown) -> (0,-100)
	     	(SpecialKey KeyLeft) -> (-100,0)
	     	(SpecialKey KeyRight) -> (100,0)
	in Jogo (Jogador p) (mapaInicial)


event :: Event -> World -> World
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, i, pont) = (ModoJogo, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, i, pont) = (Opcoes Sair, jogo, i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, i, pont) = (Opcoes Sair, jogo, i, pont)
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, i, pont) = (Opcoes Jogar, jogo,i, pont)
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,i, pont) = (Opcoes Jogar, jogo,i, pont)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, i, pont) = error "Jogo Fechou"
--contiunar a jogar depois de vencer
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo, i ,pont) = estadoInicial i
--identificar que acabou o jogo
event _ (ModoJogo, (Jogo(Jogador (1000,y)) (mapaInicial)),i,pont) = (PerdeuJogo,(Jogo(Jogador (1000,y)) (mapaInicial)), i, pont)
event _ (ModoJogo, (Jogo(Jogador (-1000,y)) (mapaInicial)),i,pont) = (PerdeuJogo,(Jogo(Jogador (-1000,y)) (mapaInicial)), i, pont)
event _ (ModoJogo, (Jogo(Jogador (x, 525)) (mapaInicial)),i,pont) = (PerdeuJogo,(Jogo(Jogador (x,525)) (mapaInicial)), i, pont)
event _ (ModoJogo, (Jogo(Jogador (x,-725)) (mapaInicial)),i,pont) = (PerdeuJogo,(Jogo(Jogador (x,-725)) (mapaInicial)), i, pont)
--Modo Jogo
event (EventKey key Down _ _) (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = (ModoJogo, novoEstado key (x,y), i, pont)
--Andar para tras com o tempo
event _ (ModoJogo, (Jogo (Jogador (x,y)) (mapaInicial)), i, pont) = (ModoJogo, (Jogo(Jogador (x,y)) (mapaInicial)),i, pont)
--Nao reagir caso não aconteçam os casos em cima
event _ x = x

pontu :: Float -> World -> World
pontu p (PerdeuJogo, j, i, pont) = (PerdeuJogo, j, i, pont)
pontu p (o,j,i,pont) = (o,j,i,pont+p)

main :: IO ()
main = do
	pacman <- loadBMP "pac_open.bmp"
	let imagem = [scale 4 4 pacman]
	play window (greyN 0.6) fr (estadoInicial imagem) desenhaEstado event pontu
























