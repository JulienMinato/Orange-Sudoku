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
position :: (Int,Int) -> Bool
--diffcolor (ix,iy) = (ix /= iy)

position (1,2) = True
position (1,3) = True
position (2,4) = True
position (2,5) = True
position (3,4) = True
position (3,5) = True
position (4,5) = True

position (2,1) = True
position (3,1) = True
position (4,2) = True
position (5,2) = True
position (4,3) = True
position (5,3) = True
position (5,4) = True

position (ix,iy) = not (ix /= iy)




graphcoloringPrint :: [State] -> String
graphcoloringPrint [] = "No solution"

                    else '*':(helper (c+1) x n)

--graphcoloringTest = 5 [(1,2)-> True, (1,3)-> True, (2,3)->True] 3 

-- 5 place, Wa, Or, Nt, Q, Ny 
-- 3 color red green blue
-- red = 1, green = 2, blue = 3 