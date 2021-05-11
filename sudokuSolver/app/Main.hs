import CSP
import NQueens

import Control.Exception
import System.Environment
import Data.Maybe

getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn


main :: IO ()
main = do
    putStrLn "N queens puzzle: how many queens?"
    n <- getNum               
    print (solver (queens n))
