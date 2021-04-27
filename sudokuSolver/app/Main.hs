{-

This is an attempt to implement in Haskell, Peter Norvig's sudoku  
solver :
"Solving Every Sudoku Puzzle" (http://norvig.com/sudoku.html)

In Norvig's program, methods which change a grid return either a new  
grid, either False (failure).
Here I use Maybe, and return Just grid or Nothing in case of failure

-}

module Main where
	
import Prelude hiding (lookup)
import Data.List hiding (lookup)
import qualified Data.Map as M
import Control.Monad
import Maybe
import System.IO

--------------------------------------------------
-- Types
type Digit  = Char
type Square = String
type Unit   = [Square]

-- We represent our grid as a Map
type Grid = M.Map Square [Digit]


--------------------------------------------------
-- Setting Up the Problem

rows = "ABCDEFGHI"
cols = "123456789"
digits = "123456789"

cross :: String -> String -> [String]
cross rows cols = [ r:c:[] | r <- rows, c <- cols ]

squares :: [Square]
squares = cross rows cols  -- ["A1","A2","A3",...]

unitlist :: [Unit]
unitlist = [ cross rows [c] | c <- cols ] ++
            [ cross [r] cols | r <- rows ] ++
            [ cross rs cs | rs <- ["ABC","DEF","GHI"], cs <-  
["123","456","789"]]

units :: M.Map Square [Unit]
units = M.fromList [ (s, [ u | u <- unitlist, elem s u ]) | s <-  
squares ]

peers :: M.Map Square [Square]
peers = M.fromList [ (s, set [[ p | p <- e, p /= s ] | e <- lookup s  
units ]) | s <- squares ]
   where set = nub . concat

--------------------------------------------------
-- Wrapper around M.lookup used in list comprehensions

lookup :: (Ord a, Show a) => a -> M.Map a b -> b
lookup k v = case M.lookup k v of
                 Just x -> x
                 Nothing -> error $ "Error : key " ++ show k ++ " not  
in map !"

-- lookup k m = fromJust . M.lookup k m
--------------------------------------------------
-- Parsing a grid into a Map

parsegrid     :: String -> Maybe Grid
parsegrid g    = do regularGrid g
                     foldM assign allPossibilities (zip squares g)

   where  allPossibilities :: Grid
          allPossibilities = M.fromList [ (s,digits) | s <- squares ]
          regularGrid   :: String -> Maybe String
          regularGrid g  = if all (\c -> (elem c "0.-123456789")) g
                              then (Just g)
                              else Nothing

--------------------------------------------------
-- Propagating Constraints

assign        :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) = if (elem d digits) then do -- check that we are  
assigning a digit and not a '.'
                     let toDump = delete d (lookup s g)
                     res <- foldM eliminate g (zip (repeat s) toDump)
                     return res
                  else return g

eliminate     ::  Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) = let cell = lookup s g in
                     if not (elem d cell) then return g -- already  
eliminated
                     -- else d is deleted from s' values
                        else do let newCell = delete d cell
                                    newV = M.insert s newCell g --
                                newV2 <- case length newCell of
                                            -- contradiction :  
Nothing terminates the computation
                                            0 -> Nothing
                                            -- if there is only one  
value (d2) left in square, remove it from peers
                                            1 -> do let peersOfS =  
[ s' | s' <- lookup s peers ]
                                                    res <- foldM  
eliminate newV (zip peersOfS (cycle newCell))
                                                    return res
                                            -- else : return the new  
grid
                                            _ -> return newV
                                -- Now check the places where d  
appears in the units of s
                                let dPlaces = [ s' | u <- lookup s  
units, s' <- u, elem d (lookup s' newV2) ]
                                case length dPlaces of
                                   0 -> Nothing
                                   -- d can only be in one place in  
unit; assign it there
                                   1 -> assign newV2 (head dPlaces, d)
                                   _ -> return newV2


--------------------------------------------------
-- Search

search         :: Maybe Grid -> Maybe Grid
search Nothing  = Nothing
search (Just g) = if all (\xs -> length xs == 1) [ lookup s g | s <-  
squares ]
                     then (Just g) -- solved
                     else do let (_,s) = minimum [ (length (lookup s  
g),s) | s <- squares, length (lookup s g) > 1 ]
                                 g' = g -- copie of g
                             foldl' some Nothing [ search (assign  
g' (s,d)) | d <- lookup s g ]
   where some Nothing Nothing  = Nothing
         some Nothing (Just g) = (Just g)
         some (Just g) _ = (Just g)


--------------------------------------------------
-- Display solved grid

printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString

gridToString :: Grid -> String
gridToString g = let l0= map snd (M.toList g)                 --  
[("1537"),("4"),...]
                      l1 = (map (\s -> " " ++ s ++ " ")) l0    -- ["  
1 "," 2 ",...]
                      l2 = (map concat . sublist 3) l1         -- ["  
1  2  3 "," 4  5  6 ",...]
                      l3 = (sublist 3) l2                      -- [["  
1  2  3 "," 4  5  6 "," 7  8  9 "],...]
                      l4 = (map (concat . intersperse "|")) l3 -- ["  
1  2  3 | 4  5  6 | 7  8  9 ",...]
                      l5 = (concat . intersperse [line] . sublist 3) l4
                   in unlines l5
   where sublist n [] = []
         sublist n xs = take n xs : sublist n (drop n xs)
         line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
         hyphens = take 9 (repeat '-')

--------------------------------------------------



main :: IO ()
main = do h <- openFile "top95.txt" ReadMode
           grids <- hGetContents h
           let solved = mapMaybe (search . parsegrid) (lines grids)
           mapM_ printGrid solved
           hClose h
