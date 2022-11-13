{- |
Module      : Tarefa1_2022li1g079
Description : Validação de um mapa
Copyright   : Leonor Cunha <a103997@alunos.uminho.pt>
              Tiago Barros <a104530@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g079 where

import LI12223

{-A função mapaValido é a função principal e funciona a partir da junção das várias funções auxiliares. (Se o resultado de todas as funções auxiliares for Verdadeiro, então a função mapaValido também o será.)-}

mapaValido :: Mapa -> Bool
mapaValido (Mapa c ((tr, (x:y)):t)) = mapaValido1 (Mapa c ((tr, (x:y)):t)) && mapaValido2 (Mapa c ((tr, (x:y)):t)) && mapaValido3 (Mapa c ((tr, (x:y)):t)) && mapaValido4 (Mapa c ((tr, (x:y)):t)) && mapaValido5 (Mapa c ((tr, (x:y)):t)) && mapaValido6 (Mapa c ((tr, (x:y)):t)) && mapaValido7 (Mapa c ((tr, (x:y)):t))

{- A função mapaValido1 certifica que não existem obstáculos em terrenos impróprios. 
Na primeira linha tem o caso de paragem, onde se a lista de [Terreno, (Obstáculos)] está vazia e por isso não há erros.
Nas linhas seguintes é determinado o que fazer no caso dos três terrenos possíveis. Na estrada, os obstáculos possíveis são carros, na relva, as árvores e nos rios, troncos.
Caso algum dos elementos que não pertence ao respetivo terreno se encontrar na lista de obstáculos (utilizando a função elem), o mapa deixa de ser válido. Por exemplo, não podem existir nem árvores, nem carros no rio.
Em todos os casos de verificação dos terrenos com os obstáculos existe um otherwise no caso de não serem encotrados obstáculos impróprios, permitindo verificar a totalidade do mapa e aferir a sua veracidade ou não.
A função retornara um Bool para depois ser analisado na função principal.
-}

mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa c []) = True
mapaValido1 (Mapa c (((Estrada n),(x:y)):t)) 
    |(elem Arvore (x:y) || elem Tronco (x:y)) = False
    |otherwise = mapaValido1 (Mapa c t)
mapaValido1 (Mapa c (((Rio n),(x:y)):t)) 
    |(elem Carro (x:y) || elem Arvore (x:y)) = False
    |otherwise = mapaValido1 (Mapa c t)     
mapaValido1 (Mapa c (( Relva, (x:y)):t)) 
    |(elem Tronco (x:y) || elem Carro (x:y)) = False
    | otherwise = mapaValido1 (Mapa c t)

{-A função mapaValido2 verifica se existem rios contíguos com a mesma direção (o que não pode acontecer).
Os rios, assim como a estrada, tem velocidade associada ao terreno pois estes tem movimento. Se a velocidade for um valor positivo então os obstáculos movem-se da esquerda para a direita, se for negativo é o contrário.
Para verificar se os rios contíguos tem a mesma direção, avaliamos a velocidade (se são valores ambos positivos, ambos negativos ou os dois diferentes).
Na primeira linha, assim como na função mapaValido1, temos o caso de paragem onde a lista [Terreno, (Obstáculos)] está vazia e por isso não há erros.
Na segunda linha, caso a função receba um mapa que só tenha um rio, ou seja, sem mais nenhum rio a seguir, o mapa estará válido pois não tem nenhum rio contígui, podendo ter qualquer velocidade.
De seguida, nas linhas 3 e 4, caso receba como primeiro terreno estrada ou relva, irá avaliar o resto da lista pois não existe nenhuma restrição para esses terrenos nesta função.
Caso a função receba primeiramente o terreno rio mas o segundo terreno é relva ou estrada, a função irá verificar o resto da lista, não havendo nenhum problema nestas combinações.
Por fim, no caso da função receber dois rios seguidos, esta vai verificar se as velocidades tem valores iguais, ou seja, no caso do mapa nao ser válido.
Se os valores forem contrários (positivo, negativo ou o contrário) o mapa será válido (até aquele caso) e a função verifica o resto da lista. 
-}

mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa c []) = True
mapaValido2 (Mapa c ((Rio n, (x:y)):[])) = True
mapaValido2 (Mapa c ((Relva, (x:y)):t)) = mapaValido2 (Mapa c t)
mapaValido2 (Mapa c ((Estrada n, (x:y)):t)) = mapaValido2 (Mapa c t)
mapaValido2 (Mapa c ((Rio n, (x:y)): (Relva,  (x':y')):t)) = mapaValido2 (Mapa c t)
mapaValido2 (Mapa c ((Rio n, (x:y)): (Estrada n', (x':y')):t)) = mapaValido2 (Mapa c t) 
mapaValido2 (Mapa c ((Rio n, (x:y)): (Rio n', (x':y')): t))
    | n > 0  && n' >0 = False
    | n < 0 && n' < 0 = False
    |otherwise = mapaValido2 (Mapa c ((Rio n', (x':y')):t))

{-Na função mapaValido3 verifamos que os troncos não tem mais de 5 unidades.
Como nas funções vistas até agora, o caso de paragem na primeira linha acontece por não ter erro nenhum, a lista de obstáculos está vazia.
No caso de o primeiro terreno ser relva ou estrada, que não estamos a avaliar nesta função, a função irá avaliar o resto da lista (Terreno,[Obstáculo]) (recursividade).
Quando o primeiro terreno coincide com Rio, a partir de duas funções auxiliares que criamos (conta e contaAux - agrupa elementos iguais e consecutivos numa lista; 
procura e devolve o numero de elementos com maior numero de Troncos das listas resultantes da primeira função (respetivamente)), se existirem troncos com mais de 5 unidades, o mapa não 
será válido pois não cumpre os requisitos. Se não existir nenhum caso onde existem troncos com mais de cinco unidades então, verificamos o resto da lista e se nada ocorrer em contrário,
o mapa é válido.
-}

mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa c []) = True
mapaValido3 (Mapa c ((Relva, (x:y)):t)) = mapaValido3 (Mapa c t)
mapaValido3 (Mapa c ((Estrada n, (x:y)):t)) = mapaValido3 (Mapa c t) 
mapaValido3 (Mapa c ((Rio n, (x:y)):t)) | contaAux (conta (x:y)) > 5 = False
                                        | otherwise = mapaValido3 (Mapa c t)


conta :: [Obstaculo] -> [[Obstaculo]] 
conta [] = [] 
conta [x] = [[x]]
conta (x:y:t) | elem x (head r ) = (x:(head r)) : tail r
              | otherwise = [x] :r
                where r = conta (y:t)

contaAux :: [[Obstaculo]] -> Int
contaAux [x] = length x
contaAux [] = 0
contaAux ((x:y):t) | (elem Tronco (x:y)) && (length (x:y) >= (contaAux t)) = length (x:y)
                   | otherwise = contaAux t

{-Pela mesma lógica que a função mapaValido3, para função mapaValido4, que pretende verificar se não existem carros com mais de três unidades, utilizamos a função auxilar conta (criada em cima) e
a função contaAux1 que é uma variação da função contaAux mas procura e devolve o numero de elementos com maior numero de Carros, e não Troncos, das listas resultantes da primeira função.
Na primeira linha está o caso de paragem semelhante ás funções vistas até ao momento (se a lista não contém nada, não pode dar erro).
De seguida definimos os casos em que o primeiro terreno não é estrada, ou seja, relva ou rio, em que a função irá apenas verificar o resto da lista.
Quando a função encontra um terreno estrada (utiliza as funções auxiliares) verifica se o número de carros seguidos nessa lista é superior a três. Se sim, o mapa não é válido, se não então verificamos 
a validade do resto da lista.
-}

mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa c []) = True
mapaValido4 (Mapa c ((Relva, (x:y)):t)) = mapaValido4 (Mapa c t)
mapaValido4 (Mapa c ((Rio n, (x:y)):t)) = mapaValido4 (Mapa c t) 
mapaValido4 (Mapa c ((Estrada n, (x:y)):t)) | contaAux1 (conta (x:y)) > 3 = False
                                            | otherwise = mapaValido4 (Mapa c t)


contaAux1 :: [[Obstaculo]] -> Int
contaAux1 [x] = length x
contaAux1 [] = 0
contaAux1 ((x:y):t) | (elem Carro (x:y)) && (length (x:y) >= (contaAux1 t)) = length (x:y)
                    | otherwise = contaAux1 t                                        
                    
{- mapaValido5 verifica se existe alguma linha do mapa constituída exclusivamente por obstáculos sem espaços vazios (Nenhum).
Caso de paragem na primeira linha, caso a lista se vazia o mapa é válido.
Quando recebe um mapa, procura se existe algum elemento na lista dos obstáculos (independetemente do terreno) que seja Nenhum. Se não existir, o mapa não é válido (False == False).
Por outro lado, se existir, a função irá verificar o resto da lista no intuito de validar ou não o mapa.
-}

mapaValido5 :: Mapa -> Bool 
mapaValido5 (Mapa c []) = True
mapaValido5 (Mapa c ((tr, (x:y)):t))| elem Nenhum (x:y) == False = False
                                    | otherwise = mapaValido5 (Mapa c t)

{-A função mapaValido6 compara a largura do mapa com o número de elementos dos obstáculos de cada terreno/linha (comprimento da lista). 
O caso de paragem irá dar mapa válido pois a lista está vazia.
Quando recebe um Mapa (onde a lista não está vazia), a função vai comparar a largura atribuida ao Mapa (c) com o comprimento da lista dos obstáculos. Se forem diferentes, o mapa vai dar inválido,
uma vez que não podem haver obstáculos fora dos limites do mapa e toda a largura do mapa deve estar definida até os espaços vazios (Nenhum).
Se no primeiro terreno/ lista, não houver erros/problemas/a largura for igual ao número de elementos, então a função irá verificar o resto da lista e depois, consoante o resultado (tudo verdadeiro -> mapa válido)
vai aferir se o mapa é válido ou não.
-}

mapaValido6 :: Mapa -> Bool
mapaValido6 (Mapa c []) = True
mapaValido6 (Mapa c ((tr, (x:y)):t)) | c /= length (x:y) = False
                                     | otherwise = mapaValido6 (Mapa c t)


{- A função mapaValido7 verifica se exitem mais de 4 rios seguidos e/ou mais de 5 Estradas ou Relva.
O caso de paragem é o mesmo das outras funções, uma vez que a lista vazia cumpre a condição.
Para os vários terrenos utilizamos uma funçao auxiliar que agrupa em listas os elementos iguais de terrenos e obstáculos iguais do mapa dado (conta 7).
Na funçao mapaValido7 para cada terreno definimos as condiçoes. No geral, comparamos o comprimento da primeira lista criada em conta7 com a condiçao possivel para cada terreno
(ex: relva e estrada nao podem haver mais que 5 seguidos e no rio mais de 4 seguidos), caso nada contrarie a condiçao nessa lista entao a funçao avalia o resto das listas criadas na conta7.
-}


mapaValido7 :: Mapa -> Bool
mapaValido7 (Mapa c []) = True
mapaValido7 (Mapa c ((Estrada n, (x:y)):t)) | (length (head (conta7 ((Estrada n, (x:y)):t)))) > 5 = False
                                            | otherwise = mapaValido7 (Mapa c t)

mapaValido7 (Mapa c ((Rio n, (x:y)):t)) | (length (head (conta7 ((Rio n, (x:y)):t)))) > 4 = False
                                        | otherwise = mapaValido7 (Mapa c t)

mapaValido7 (Mapa c ((Relva, (x:y)):t)) | (length (head (conta7 ((Relva, (x:y)):t)))) > 5 = False
                                        | otherwise = mapaValido7 (Mapa c t)

conta7 :: [(Terreno,[Obstaculo])] -> [[(Terreno, [Obstaculo])]] 
conta7 [] = [] 
conta7 [x] = [[x]]
conta7 ((x,y):(x',y'):t) | x == x' = ((x,y):(head r)) : tail r
                         | otherwise = [(x,y)] :r
                         where r = conta7 ((x',y'):t)
