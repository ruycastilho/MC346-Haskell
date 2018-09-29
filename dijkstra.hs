--Estrutura de um vertice: (peso do vertice, 'indice do no', lista de adjacencia, 'indice do pai'////////////)
--Estrutua da lista de adjacencia: [(peso da aresta, 'indice do no'), ...]
--Estrutura da lista de vertices s: [(peso do vertice, 'indice do vertice', pai do vertice), ...]

--obtem o terceiro elemento de uma tupla
trd (_,_,a,_,_) = a

----obtem o quarto elemento de uma tupla
for(_,_,_,a,_) = a

--funcao relax
--(nu, pu, lau, piu) = u -> vertice u cuja lista de adjacencia esta sendo relaxada
--(nv, pv) = v -> vertice adjacente a u que esta sendo relaxado
--(nx, px, lax, pix):xs = g -> grafo g
--Exemplo relax (5, 'u', [(2, 'v')], ' ') (2, 'v') [(9, 'v', [(3, 'j')], ' '), (3, 'a', [(5, 'r')], ' '), (10, 'b', [(3, 't')], ' ')]
relax _ _ [] = []
relax (pu, nu, lau, piu, typeu) (pv, nv) ((px, nx, lax, pix, typex):xs)
  |nv == nx = if(px > (pu + pv)) then ((pu + pv), nx, lax, nu, typex):xs else (px, nx, lax, pix, typex):xs
  |otherwise = (px, nx, lax, pix, typex):(retorno)
    where
      retorno = relax (pu, nu, lau, piu, typeu) (pv, nv) xs

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
