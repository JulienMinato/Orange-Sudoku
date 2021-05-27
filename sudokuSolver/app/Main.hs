import CSP
import NQueens
import Sudoku
import Graphcoloring

import Control.Exception
import System.Environment
import Data.Maybe
import Control.Monad
import Control.Monad.CSP
import System.IO.Unsafe
import Data.List


--input Int 
getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn


loadSudoku :: [(Int, [[Int]])]
--loadSudoku = let f = unsafePerformIO $ readFile "sudoku50.txt"
loadSudoku = let f = unsafePerformIO $ readFile "/Users/liang/Projects/GitHub/Orange-Sudoku/sudokuSolver/app/sudoku50.txt"
      in map (\(g:gs) -> (read $ drop 5 g, solveSudoku $ map (\g -> map (read . (:[])) g) gs))
             $ groupBy (\a b -> not $ isPrefixOf "Grid" b) $ lines f



queensResult :: IO()
queensResult = do
    putStrLn "N queens puzzle: how many queens?"
    n <- getNum    
    putStr (nQueenPrint (solver (queens n))) 

sudokuResult :: IO()
sudokuResult = do
    putStrLn "\n\nSudoku Question1\n\n"
    mapM_ print sudokuTest
    --putStrLn"\nImport a Sudoku example from a file: \n"
    putStrLn "\n\nSudoku Question2\n\n"
    
    mapM_ print sudokuTest2
    putStrLn "\n\nThe Solution1\n\n"
    --i <- readFile "/Users/liang/Projects/GitHub/Orange-Sudoku/sudokuSolver/app/sudoku.txt" 
    --putStrLn i
    --i <- readFile "/Users/liang/Projects/GitHub/Orange-Sudoku/sudokuSolver/app/data/top95.txt" 
    --i <- readFile "sudoku.txt" 
    --prettyprint . lines $ i
    --load [1..9] i 
    mapM_ print (solveSudoku sudokuTest)
    putStrLn "\n\nThe Solution2\n\n"
    mapM_ print (solveSudoku sudokuTest2)


graphcolResult :: IO()
graphcolResult = do 
    putStrLn "an instance of a graph coloring problem (Kempe, 1879)\n"

    --mapM_ print (solver (graphcoloring 5 position 3))
    putStr (graphColorPrint (solver (graphcoloring 5 position 3)))

testResult :: IO()
testResult = do
    putStrLn "test\n"
    putStrLn"\nImport a Sudoku example from a file: \n"
    --i <- readFile "/Users/liang/Projects/GitHub/Orange-Sudoku/sudokuSolver/app/sudoku.txt"
    --putStrLn i
    mapM_ print loadSudoku
    --print read i :: Integer
    --solver (graphcoloring 5 diffcolor 3)








main :: IO ()
main = do
    

    putStrLn "\n\n\n ******Which CSPs problem do you want to test?******\n"
    

    putStrLn "Please enter a number: \n\n"

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
        _ -> putStrLn "***Wrong input, please try again\n"
    -- print (solver (queens n))
