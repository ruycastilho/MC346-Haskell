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
    let graph = makegraph travelTimes [] (fst endPoints)
    -- let graph2 = addBusLine travelTimes graph

    putChar '\n'
    putStrLn "Travel times:"
    print travelTimes
    putStrLn "\nBus Lines:"
    print busLines
    putStrLn "\nEnd Points:"
    print endPoints
    putStrLn "\nGraph:"
    print graph
    -- putStrLn "\nAdd Bus Lines:"
    -- print graph2

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
makegraph [] graph _ = graph
makegraph (x:xs) graph start
  |transport == "a-pe" = makegraph xs (insere_a_pe x graph start) start
    where
      (begin, end, transport, time) = x

insere_a_pe (begin, end, transport, time) graph start
  |graph == [] = if (begin == start) then [(0, begin, [(time, end, transport)], "", "")]
                                     else [(1000, begin, [(time, end, transport)], "", "")]
  |begin == beginx = ((weight, beginx, (time, end, transport):adjacency, father, ""):xs)
  |otherwise = (weight, beginx, adjacency, father, transportx):(insere_a_pe (begin, end, transport, time) xs start)
    where
      ((weight, beginx, adjacency, father, transportx):xs) = graph

-- insertBusLine _ [] = [get_in, get_out]
-- insertBusLine (begin, end, transport, time) (x:xs)
--   |"*"++begin == node = (weight, node, ((time, ("*"++end), transport):adjacency), father, transport):xs
--   |"*"++end == node =
--     where
--       get_in = (1000, ("*"++begin), [(0, begin, ""), (time, ("*"++end), transport)], "", "")
--       get_out = (1000, ("*"++end), [(0, end, "")], "", "")
--       (weight, node, adjacency, father, transport) = x






















--Atualiza os trajetos com onibus, para considerar o custo de subida no onibus
-- attTimeBus [] graph = graph
-- attTimeBus (x:xs) (y:ys) = attTimeBus xs ys
--   where
--     (weight, begin, adjacency, father, transport) = y
--     adjacency_att = (verifyAdjacencyList x adjacency)
--
-- verifyAdjacencyList _ [] = []
-- verifyAdjacencyList (name, time) (timex, node, nameLine):xs
--   |name == nameLine = ((timex + (time / 2)), node, nameLine):xs
--   |otherwise = (timex, node, nameLine):(verifyAdjacencyList (name, time) xs)

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
