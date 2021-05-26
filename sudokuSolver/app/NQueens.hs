-- | Example: N-Queens Problem
module NQueens where

import CSP

--The n-queens problem looks for a way to put n queens on a n × n chess board such that no queen is threatening another. 

-- The CSP variables are the columns, the values are the rows, 
-- and each assignment represents the placement of a single queen; 
-- the oracle function replies True on a pair of queen positions provided that the queens are on different rows 
-- and on different diagonals. 


queens :: Int -> CSP
queens n = CSP{vals=n,vars=n,rel=safe}
  where safe (col1 := row1) (col2 := row2) = (row1 /= row2) && abs (col1 - col2) /= abs (row1 - row2)


nQueenPrint :: [State] -> String
nQueenPrint [] = ""
nQueenPrint (x:xs) = let State(a,_) = x
                     in assignPrint a (length a) ++ "\n" ++ "\n" ++ nQueenPrint xs
                          --show '\n
assignPrint :: [Assignment] -> Int -> String
assignPrint []     _ = ""
assignPrint (x:xs) n = let _ := b = x
                       in helper 1 b n ++ "\n" ++ assignPrint xs n

helper :: Int -> Int -> Int -> String
helper c x n = if c == x
               then 'Q':(helper (c+1) x n)
               else if c > n
                    then " "
                    else '*':(helper (c+1) x n)

--prettyprintNQueens :: CSP -> [Int, Int]
--prettyprintNQueens _ = [] 
