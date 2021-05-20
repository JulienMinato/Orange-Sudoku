import CSP
import NQueens
import Sudoku

import Control.Exception
import System.Environment
import Data.Maybe
import Control.Monad

--input Int 
getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn


main :: IO ()
main = do
    
    i <- readFile "/Users/liang/Projects/GitHub/Orange-Sudoku/sudokuSolver/app/sudoku.txt" 
    prettyprint . lines $ i
    --load [1..9] i 
    -- putStrLn "N queens puzzle: how many queens?"
    -- n <- getNum               
    -- print (solver (queens n))
