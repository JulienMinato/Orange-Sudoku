module Graphcoloring where

import CSP

-- The graphcoloring function constructs an instance of a graph coloring, 
-- specified by a number of nodes, a set of edges between nodes
--  (represented by a characteristic function on pairs of nodes),
--   and a number of colors. The CSP variables are the graph nodes, 
--   the values are the possible colors, and the oracle function returns True on 
--   a pair of color assignments provided that the colors are different or there is no edge between the nodes.



graphcoloring :: Int -> ((Var,Var) -> Bool) -> Int -> CSP
graphcoloring nodes adj colors = CSP{vars=nodes,vals=colors,rel=ok}
        where ok (n1 := c1) (n2 := c2) = c1 /= c2 || not (adj (n1,n2))


--adj = [(1,2)-> True, (1,3)-> True, (2,3)->True]
diffcolor :: ((Var,Var) -> Bool)

diffcolor (1,2) = True
diffcolor (1,3) = True
diffcolor (2,3) = True
--graphcoloringTest = 5 [(1,2)-> True, (1,3)-> True, (2,3)->True] 3 

-- 5 place, Wa, Or, Nt, Q, Ny 
-- 3 color red green blue
-- red = 1, green = 2, blue = 3 