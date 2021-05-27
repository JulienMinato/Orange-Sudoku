module Sudoku where

-- | Example: Sudoku solver Problem

import CSP


import Data.Char
import Control.Monad
import Control.Applicative 
import Control.Monad.CSP


import Data.Maybe
import Data.List


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


sudokuTest2 =   [[0,0,3,0,2,0,6,0,0],
                [9,0,0,3,0,5,0,0,1],
                [0,0,1,8,0,6,4,0,0],
                [0,0,8,1,0,2,9,0,0],
                [7,0,0,0,0,0,0,0,8],
                [0,0,6,7,0,8,2,0,0],
                [0,0,2,6,0,9,5,0,0],
                [8,0,0,2,0,3,0,0,9],
                [0,0,5,0,1,0,3,0,0]]


-- this defination ideas from https://wiki.haskell.org/Sudoku

type PuzzleValue = Int
type Cell = (Int, Int) -- One-based coordinates

type Puzzle  = [[Maybe PuzzleValue]]
type Solution = [[PuzzleValue]]

-- The size of the puzzle.
sqrtSize :: Int
sqrtSize = 3

size = sqrtSize * sqrtSize

-- Besides the rows and columns, a Sudoku puzzle contains s blocks
-- of s cells each, where s = size.
blocks :: [[Cell]]
blocks = [[(x + i, y + j) | i <- [1..sqrtSize], j <- [1..sqrtSize]] |
         x <- [0,sqrtSize..size-sqrtSize],
         y <- [0,sqrtSize..size-sqrtSize]]


-- The one-based number of the block that a cell is contained in.
blockNum :: Cell -> Int
blockNum (row, col) = row - (row - 1) `mod` sqrtSize + (col - 1) `div` sqrtSize




-- sudoku :: [[Int]] -> CSP
-- sudoku = undefined

-- sudoku puz = CSP{vals=n,vars=n,rel=ok}
--   where ok (col1 := row1) (col2 := row2) = (row1 /= row2) && abs (col1 - col2) /= abs (row1 - row2)

-- sudoku :: [Int] -> CSP
-- sudoku puz = CSP{vals=[1..81],vars=9,rel=ok}

--  where ok (pos1 := row1) (pos2 := row2) = row1 /= row2 && col1 /= col2

--[0,3,4,0,5,9,0,0,0]

--load from the string (file IO)
load :: [Int] -> String -> Puzzle
load =undefined
-- load xs s = do
--              (line,s1)   <- zip xs $ lines s
--              (column,c1) <- zip xs s1 
--              case c1 of 
--                 '.' -> return [[0]]
--                 c   -> let i = digitToInt c in return [[just i]]]

--Test sudoku
--putStr solver sudoku sudokuTest






--External CSP library to solve Sudoku problems
--using csp: Discrete constraint satisfaction problem (CSP) solver.
-- Control.Monad.CSP
--from https://hackage.haskell.org/package/csp



solveSudoku :: (Enum a, Eq a, Num a) => [[a]] -> [[a]]
solveSudoku puzzle = oneCSPSolution $ do
  dvs <- mapM (mapM (\a -> mkDV $ if a == 0 then [1 .. 9] else [a])) puzzle
  mapM_ assertRowConstraints dvs
  mapM_ assertRowConstraints $ transpose dvs
  sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
  return dvs
      where assertRowConstraints =  mapAllPairsM_ (constraint2 (/=))
            assertSquareConstraints dvs i j = 
                mapAllPairsM_ (constraint2 (/=)) [(dvs !! x) !! y | x <- [i..i+2], y <- [j..j+2]]



mapAllPairsM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
mapAllPairsM_ f []     = return ()
mapAllPairsM_ f (_:[]) = return ()
mapAllPairsM_ f (a:l) = mapM_ (f a) l >> mapAllPairsM_ f l




-- Replace one element of a list.
-- Coordinates are 1-based.


replace :: Int -> a -> [a] -> [a]
replace i x (y:ys)
    | i > 1     = y : replace (i - 1) x ys
    | otherwise = x : ys
replace _ _ _ = []

-- Replace one element of a 2-dimensional list.
-- Coordinates are 1-based.
replace2 :: Int -> Int -> a -> [[a]] -> [[a]]
replace2 i j x (y:ys)
    | i > 1     = y : replace2 (i - 1) j x ys
    | otherwise = replace j x y : ys
replace2 _ _ _ _ = []




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




