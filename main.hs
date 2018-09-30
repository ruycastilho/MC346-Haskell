-- MC 346 - Haskell Project
-- Group:
--  Ruy Castilho Barrichelo RA 177012
--  Vitor Kaoru Aoki        RA 178474

import Data.Char
import Data.Map
import Data.List as L
import Data.Maybe

-- Main e I/O
main = do
    travelTimes <- getTravelTimeInput []
    busLines <- getBusLineInput []
    endPoints <- getEndPointsInput
    let graph = makegraph travelTimes [] (fst endPoints) busLines
    let result_dikjstra = dijkstra graph
    let (path, time) = parseOutput result_dikjstra endPoints
    let final = dropStar path "" "" (snd endPoints)

    sequence $ L.map putStr (L.intersperse " " final)
    putStrLn ""
    print time

getTravelTimeInput travelTimes = do
        travelTimeInput <- getLine
        if Prelude.null travelTimeInput
            then return travelTimes
            else do
                let [a,b,c,d] = L.words travelTimeInput
                getTravelTimeInput ((a,b,c, read d :: Float):travelTimes)

getBusLineInput busLines= do
    busLineInput <- getLine
    if Prelude.null busLineInput
        then return busLines
        else do
            let [a,b] = L.words busLineInput
            getBusLineInput ((a, (read b :: Float)/2):busLines)

getEndPointsInput = do
    endPointsInput <- getLine
    let [a, b] = L.words endPointsInput
    return (a, b)

-- Dijkstra
--Estrutura de um vértice: (peso do vértice, indice do no, lista de adjacencia, indice do pai, tipo de transporte)
--Estrutura da lista de adjacência: [(peso da aresta, indice do no, tipo de transporte), ...]
--Estrutura da lista de vértices s: [(peso do vertice, indice do vertice, lista de adjacencia, indice do pai, tipo de transporte), ...]

--Constroi a lista de vértices do grafo para ser utilizada no Dijkstra
makegraph [] graph _ _= graph
makegraph (x:xs) graph start busLines
  |transport == "a-pe" = makegraph xs (insert_a_pe x graph start) start busLines
  |otherwise = makegraph xs (insert_bus x graph start cost_get_in) start busLines
    where
      (begin, end, transport, time) = x
      cost_get_in = find_bus busLines transport

--Insere um nó no grafo caso o meio de transporte seja a pé
insert_a_pe (begin, end, transport, time) graph start = graph_end
    where
      node_begin = if (begin == start) then (0, begin, [(time, end, transport)], "", "")
                                       else (1000, begin, [(time, end, transport)], "", "")
      node_end = (1000, end, [], "", "")
      graph_begin = insert_graph node_begin graph
      graph_end = insert_graph node_end graph_begin

--Insere um nó no grafo caso o meio de transporte seja ônibus; Cria nó artificiais da forma: <nome>*<linha>
insert_bus (begin, end, transport, time) graph start cost_get_in = graph_end_bus
  where
    node_begin = if (begin == start) then (0, begin, [(cost_get_in, begin++"*"++transport, transport)], "", "")
                                     else (1000, begin, [(cost_get_in, begin++"*"++transport, transport)], "", "")
    node_end = (1000, end, [], "", "")
    node_begin_bus = (1000, begin++"*"++transport, [(time, end++"*"++transport, transport)], "", "")
    node_end_bus = (1000, end++"*"++transport, [(0, end, transport)], "", "")
    graph_begin = insert_graph node_begin graph
    graph_end = insert_graph node_end graph_begin
    graph_begin_bus = insert_graph node_begin_bus graph_end
    graph_end_bus = insert_graph node_end_bus graph_begin_bus

--Insere um vértice na lista de vértices do grafo
insert_graph (weight, node, adjacency, father, transport) [] = [(weight, node, adjacency, father, transport)]
insert_graph (weight, node, adjacency, father, transport) (x:xs)
  |node == nodex = (weight, node, (adjacency++adjacencyx), father, transport):xs
  |otherwise = x:(insert_graph (weight, node, adjacency, father, transport) xs)
    where
      (weightx, nodex, adjacencyx, fatherx, transportx) = x

-- Função relax
-- (pu, nu, lau, piu, typeu) = u -> vertice u cuja lista de adjacencia esta sendo relaxada
-- (pv, nv, transport) = v -> vertice adjacente a u que esta sendo relaxado
-- (px, nx, lax, pix, typex):xs = g -> grafo g
relax _ _ [] = []
relax (pu, nu, lau, piu, typeu) (pv, nv, transport) ((px, nx, lax, pix, typex):xs)
  |nv == nx = if(px > (pu + pv)) then ((pu + pv), nx, lax, nu, transport):xs else (px, nx, lax, pix, typex):xs
  |otherwise = (px, nx, lax, pix, typex):(retorno)
    where retorno = relax (pu, nu, lau, piu, typeu) (pv, nv, transport) xs

-- Realiza o loop do Dijkstra onde são relaxadas todos os vértices adjacentes a u
loop_relax u [] g = g
loop_relax u (x:xs) g = loop_relax u xs g'
  where g' = relax u x g

-- Função Dijkstra
dijkstra g = dijkstra' g []

dijkstra' [] s = s
dijkstra' g s = (dijkstra' grafo_relax (minimo:s))
  where
    minimo = L.minimum g
    g_sem_minimo = Prelude.filter (/= minimo) g
    grafo_relax = loop_relax minimo (trd minimo) g_sem_minimo

-- Funções auxiliares

-- Obtem o terceiro elemento de uma tupla
trd (_,_,a,_,_) = a

-- Encontra o no atual no grafo resultante de acordo com o nome
findNode end ((value, name, adj, parent, transp):xs)
    | end == name = (value, name, adj, parent, transp)
    | otherwise = findNode end xs

-- Encontra o custo de subida no onibus
find_bus (x:xs) transport
  |(fst x) == transport = (snd x)
  |otherwise = find_bus xs transport

-- Retira os nos artificiais da saída
dropStar (x:[]) _ _ _ = [x]
dropStar (x1:x2:x3:xs) transp father end
  |(elem '*' x1) && (name == end) = [name]
  |(elem '*' x1) && ((x2 /= transp) || (name == father) || (name == x3))  = dropStar (x3:xs) x2 name end
  |(elem '*' x1) && (x2 == transp) = (name:(x2:(dropStar (x3:xs) x2 name end)))
  |otherwise = (x1:(x2:(dropStar (x3:xs) x2 x1 end)))
    where (name, _) = L.splitAt (fromJust $ elemIndex '*' x1) x1

-- Formatação da saída
parseOutput graph (start,end) = ((parseOutput' graph start parent [transp,name]),value)
    where (value, name, adj, parent, transp) = findNode end graph

parseOutput' [] _ _ result = result
parseOutput' graph start end result
    | start == end = start:result
    | otherwise = parseOutput' graph start parent (transp:(name:result))
    where (_, name, _, parent, transp) = findNode end graph
