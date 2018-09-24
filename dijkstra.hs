--Estrutura de um vertice: (peso do vertice, 'indice do no', lista de adjacencia, 'indice do pai'////////////)
--Estrutua da lista de adjacencia: [(peso da aresta, 'indice do no'), ...]
--Estrutura da lista de vertices s: [(peso do vertice, 'indice do vertice', pai do vertice), ...]

--ordena a lista de vertices
quicksort [] = []
quicksort (x:xs) = (quicksort menor) ++ [x] ++ (quicksort maior)
  where
    menor = filter (< x) xs
    maior = filter (>= x) xs

--obtem o terceiro elemento de uma tupla
trd (_,_,a,_) = a

----obtem o quarto elemento de uma tupla
for(_,_,_,a) = a

--funcao relax
--(nu, pu, lau, piu) = u -> vertice u cuja lista de adjacencia esta sendo relaxada
--(nv, pv) = v -> vertice adjacente a u que esta sendo relaxado
--(nx, px, lax, pix):xs = g -> grafo g
--Exemplo relax (5, 'u', [(2, 'v')], ' ') (2, 'v') [(9, 'v', [(3, 'j')], ' '), (3, 'a', [(5, 'r')], ' '), (10, 'b', [(3, 't')], ' ')]
relax (pu, nu, lau, piu) (pv, nv) ((px, nx, lax, pix):xs)
  |nv == nx = if(px > (pu + pv)) then ((pu + pv), nx, lax, nu):xs else (px, nx, lax, pix):xs
  |otherwise = (px, nx, lax, pix):(retorno)
    where
      retorno = relax (pu, nu, lau, piu) (pv, nv) xs

--realiza o loop do Dijkstra onde sao relaxadas todos os vertices adjacentes a u
loop_relax u [] g = g
loop_relax u (x:xs) g = loop_relax u xs g'
  where
    g' = relax u x g

--aplica Dijkstra ao grafo
--g eh o grafo
dijkstra g = dijkstra' (quicksort g) (quicksort g) []

dijkstra' [] _ s = s
dijkstra' (x:xs) g_atual s = (dijkstra' (quicksort (retira grafo_relax (x:s))) grafo_relax (x:s))
  where
    grafo_relax = loop_relax x (trd x) g_atual

    --retira do grafo os elementos ja atualizados
    retira g [] = g
    retira g (x:xs) = retira (drop_grafo x g) xs

    drop_grafo x (y:ys)
      |x == y = ys
      |otherwise = y:(drop_grafo x ys)

--[(0, 's', [(10, 't'), (5, 'y')], ' '), (100000, 't', [(1, 'x'), (2, 'y')], ' '), (100000, 'y', [(3, 't'), (9, 'x'), (2, 'z')], ' '), (100000, 'x', [(4, 'z')], ' '), (100000, 'z', [(6, 'x'), (7, 's')], ' ')]
