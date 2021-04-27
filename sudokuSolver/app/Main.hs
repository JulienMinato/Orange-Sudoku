import Sudoku

import Control.Exception
import System.Environment
import Data.Maybe


main :: IO ()
main = do h <- openFile "top95.txt" ReadMode
           grids <- hGetContents h
           let solved = mapMaybe (search . parsegrid) (lines grids)
           mapM_ printGrid solved
           hClose h



import Sudoku
import Control.Exception
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  [f] <- getArgs                           -- <1>
  file <- readFile f                       -- <2>

  let puzzles   = lines file               -- <3>
      solutions = map solve puzzles        -- <4>

  print (length (filter isJust solutions)) -- <5>
