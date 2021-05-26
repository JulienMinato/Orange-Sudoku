import CSP
import NQueens
import Sudoku
import Graphcoloring

import Control.Exception
import System.Environment
import Data.Maybe
import Control.Monad

--input Int 
getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn

queensResult :: IO()
queensResult = do
    putStrLn "N queens puzzle: how many queens?"
    n <- getNum    
    putStr (nQueenPrint (solver (queens n))) 

sudokuResult :: IO()
sudokuResult = do
    putStrLn "Sudoku\n"
    i <- readFile "/Users/liang/Projects/GitHub/Orange-Sudoku/sudokuSolver/app/sudoku.txt" 
    --i <- readFile "sudoku.txt" 
    prettyprint . lines $ i
    --load [1..9] i 



graphcolResult :: IO()
graphcolResult = do 
    putStrLn "graphcolResult\n"

    print (solver (graphcoloring 5 diffcolor 3))

testResult :: IO()
testResult = do
    putStrLn "test\n"
    --solver (graphcoloring 5 diffcolor 3)




main :: IO ()
main = do
    

    putStrLn "Which CSPs problem do you want to test?\n"

    putStrLn "***(1): N Queen Problem\n"
    putStrLn "***(2): Sudoku Solver\n"
    putStrLn "***(3): Graphcoloring Problem\n"
    putStrLn "***(0):Advanced command mode (solve CSPs)\n"

    choose <- getNum
    putStrLn "\n"
    case choose of
        1 -> queensResult
        2 -> sudokuResult
        3 -> graphcolResult
        0 -> testResult
    
    -- print (solver (queens n))
