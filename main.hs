-- MC 346
-- Haskell Project
-- Group:
--  Ruy Castilho Barrichelo RA 177012
--  Vitor Kaoru Aoki        RA 178474

import Data.Char
import Data.Map

main = do
    -- getstartFinishInput $ getBusLineInput $
    travelTimes <- getTravelTimeInput []
    busLines <- getBusLineInput []
    endPoints <- getEndPointsInput
    let graph = makegraph travelTimes [] (fst endPoints) busLines
    let d = dijkstra graph

    putChar '\n'
    putStrLn "Travel times:"
    print travelTimes
    putStrLn "\nBus Lines:"
    print busLines
    putStrLn "\nEnd Points:"
    print endPoints
    putStrLn "\nGraph:"
    print graph
    putStrLn "\nDijkstra:"
    print d

getTravelTimeInput travelTimes = do
        travelTimeInput <- getLine
        if Prelude.null travelTimeInput
            then return travelTimes
            else do
                getTravelTimeInput ((castFourthToFloat $ tuplify4 $ words travelTimeInput):travelTimes)

getBusLineInput busLines= do
    busLineInput <- getLine
    if Prelude.null busLineInput
        then return busLines
        else do
            getBusLineInput ((castSecondToFloat $ tuplify2 $ words busLineInput):busLines)

getEndPointsInput = do
    endPointsInput <- getLine
    return $ tuplify2 $ words endPointsInput

castSecondToFloat (a, b) = (a, newB/2)
    where newB = read b :: Float
castFourthToFloat (a, b, c, d) = (a, b, c, read d :: Float)

tuplify2 [x, y] = (x,y)
tuplify4 [w, x, y, z] = (w, x, y, z)

--Constroi a lista de vertices do grafo para ser utilizada no Dijkstra
makegraph [] graph _ _= graph
makegraph (x:xs) graph start busLines
  |transport == "a-pe" = makegraph xs (insert_a_pe x graph start) start busLines
  |otherwise = makegraph xs (insert_bus x graph start cost_get_in) start busLines
    where
      (begin, end, transport, time) = x
      cost_get_in = find_bus busLines transport

--Encontra o custo de subida no onibus
find_bus (x:xs) transport
  |(fst x) == transport = (snd x)
  |otherwise = find_bus xs transport

--Insere um no no grafo caso o meio de transporte seja a pe
insert_a_pe (begin, end, transport, time) graph start = graph_end
    where
      node_begin = if (begin == start) then (0, begin, [(time, end, transport)], "", "")
                                       else (1000, begin, [(time, end, transport)], "", "")
      node_end = (1000, end, [], "", "")
      graph_begin = insert_graph node_begin graph
      graph_end = insert_graph node_end graph_begin

--Insere um no no grafo caso o meio de transporte seja onibus
insert_bus (begin, end, transport, time) graph start cost_get_in = graph_end_bus
  where
    node_begin = if (begin == start) then (0, begin, [(cost_get_in, ("*"++begin), "")], "", "")
                                     else (1000, begin, [(cost_get_in, ("*"++begin), "")], "", "")
    node_end = (1000, end, [], "", "")
    node_begin_bus = (1000, ("*"++begin), [(time, ("*"++end), transport)], "", "")
    node_end_bus = (1000, ("*"++end), [(0, end, "")], "", "")
    graph_begin = insert_graph node_begin graph
    graph_end = insert_graph node_end graph_begin
    graph_begin_bus = insert_graph node_begin_bus graph_end
    graph_end_bus = insert_graph node_end_bus graph_begin_bus

--Insere um vertice na lista de vertices do grafo
insert_graph (weight, node, adjacency, father, transport) [] = [(weight, node, adjacency, father, transport)]
insert_graph (weight, node, adjacency, father, transport) (x:xs)
  |node == nodex = (weight, node, (adjacency++adjacencyx), father, transport):xs
  |otherwise = x:(insert_graph (weight, node, adjacency, father, transport) xs)
    where
      (weightx, nodex, adjacencyx, fatherx, transportx) = x

--Estrutura de um vertice: (peso do vertice, 'indice do no', lista de adjacencia, 'indice do pai', 'tipo de transporte')
--Estrutua da lista de adjacencia: [(peso da aresta, 'indice do no'), ...]
--Estrutura da lista de vertices s: [(peso do vertice, 'indice do vertice', pai do vertice), ...]

--obtem o terceiro elemento de uma tupla
trd (_,_,a,_,_) = a

--funcao relax
--(nu, pu, lau, piu) = u -> vertice u cuja lista de adjacencia esta sendo relaxada
--(nv, pv) = v -> vertice adjacente a u que esta sendo relaxado
--(nx, px, lax, pix):xs = g -> grafo g
--Exemplo relax (5, 'u', [(2, 'v')], ' ') (2, 'v') [(9, 'v', [(3, 'j')], ' '), (3, 'a', [(5, 'r')], ' '), (10, 'b', [(3, 't')], ' ')]
relax _ _ [] = []
relax (pu, nu, lau, piu, typeu) (pv, nv, transport) ((px, nx, lax, pix, typex):xs)
  |nv == nx = if(px > (pu + pv)) then ((pu + pv), nx, lax, nu, transport):xs else (px, nx, lax, pix, typex):xs
  |otherwise = (px, nx, lax, pix, typex):(retorno)
    where
      retorno = relax (pu, nu, lau, piu, typeu) (pv, nv, transport) xs

--realiza o loop do Dijkstra onde sao relaxadas todos os vertices adjacentes a u
loop_relax u [] g = g
loop_relax u (x:xs) g = loop_relax u xs g'
  where
    g' = relax u x g

--Funcao Dijkstra
dijkstra g = dijkstra' g []

dijkstra' [] s = s
dijkstra' g s = (dijkstra' grafo_relax (minimo:s))
  where
    minimo = minimum g
    g_sem_minimo = Prelude.filter (/= minimo) g
    grafo_relax = loop_relax minimo (trd minimo) g_sem_minimo

--Site que gera grafos e testa o dijkstra pra verificar se o programa ta funcionando: https://www.cs.usfca.edu/~galles/visualization/Dijkstra.html
--[(0, 's', [(10, 't'), (5, 'y')], ' ', "a-pe"), (100000, 't', [(1, 'x'), (2, 'y')], ' ', "a-pe"), (100000, 'y', [(3, 't'), (9, 'x'), (2, 'z')], ' ', "a-pe"), (100000, 'x', [(4, 'z')], ' ', "a-pe"), (100000, 'z', [(6, 'x'), (7, 's')], ' ', "a-pe")]
