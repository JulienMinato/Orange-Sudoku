-- | Example: Sudoku solver Problem
module Sudoku where


import Data.Char
import Control.Monad
import Control.Applicative 

--The sudoku solver problem looks 
--The domain is the scope of the variable which ranges across all digits between 1 and 9 
--for any variable initially (except for the variables that have already been assigned). 
--All possibilities can be evaluated and pruned based on the constraints. 




sudokuTest=[[0,0,0,0,0,0,9,0,7],
            [0,0,0,4,2,0,1,8,0],
            [0,0,0,7,0,5,0,2,6],
            [1,0,0,9,0,4,0,0,0],
            [0,5,0,0,0,0,0,4,0],
            [0,0,0,5,0,7,0,0,9],
            [9,2,0,1,0,8,0,0,0],
            [0,3,4,0,5,9,0,0,0],
            [5,0,7,0,0,0,0,0,0]]



type Nat      = Int 
type Pos      = (Nat,Nat)
--type Sudoku a = Puzzle (Maybe a) Pos

-- -- |load from an input file as type of String and Change it to a Sudoku problem
-- load :: [Int] -> String -> Sudoku Int
-- load xs s = do
--              (line,s1)   <- zip xs $ lines s
--              (column,c1) <- zip xs s1 
--              case c1 of 
--                 '.' -> return ((line,column), Nothing)
--                 c   -> let i = digitToInt c in return ((line,column),Just i)

-- --- |row constraint function : in each row, the valued filled should be different with one values existed 
-- onerow :: (Eq a) => Sudoku a -> Constraint2 (Maybe a) Pos
-- onerow sudoku = comfun sudoku (notElem) g
--                 where g p1 p2 = fst p1 == fst p2 

-- -- |column constraint function: in each column, the valued filled should be different with one values existed 
-- onecolumn :: (Eq a) => Sudoku a -> Constraint2 (Maybe a) Pos
-- onecolumn sudoku = comfun sudoku (notElem) g
--                 where g p1 p2 = snd p1 == snd p2  


-- -- |oneblock constraint function: in each 3x3 block, the valued filled should be different with one values existed 
-- oneblock :: Eq a => Sudoku a -> Constraint2 (Maybe a) Pos
-- oneblock sudoku = comfun sudoku (notElem) g
--                 where g p1 p2 = let (i1,j1) = rangefun p1 fst
--                                     (i2,j2) = rangefun p1 snd in 
--                                     fst p2 >=i1 && fst p2 <=j1 && snd p2 >=i2 && snd p2 <=j2




--sudokusolver :: [[Int]] -> CSP

--prettyprintSudoku ::CSP -> [[Int]]
--prettyprintSudoku _ = [[]]


prettyprint :: [String] -> IO ()
prettyprint = (mapM_ putStrLn) . addhframe . addvframe        

addvframe :: [String] -> [String]
addvframe s = s >>= \xs -> 
    case xs of 
         "" ->  return xs 
         _  ->  let 
                    (ys1,zs1) = splitAt 3 xs  
                    (ys2,zs2) = splitAt 3 zs1
                 in  
                    return (ys1 ++ "|" ++ ys2 ++"|" ++ zs2)

addhframe :: [String] -> [String]
addhframe s = 
    let 
        (ys1,zs1) = splitAt 3 s  
        (ys2,zs2) = splitAt 3 zs1
    in  
        ys1 ++ s2 ++ ys2 ++ s2 ++ zs2
    where s2 = ["--- --- ---"]   
