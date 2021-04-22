# Orange-Sudoku
Solving Every Sudoku Puzzle. 

Why did we do this? As computer security expert Ben Laurie has stated, Sudoku is "a denial of service attack on human intellect". Many people we know were infected by the virus, and we thought maybe this would demonstrate that they didn't need to spend any more time on Sudoku. You may see a lot of Sudoku behind magazines or newspapers such as the Baro. Don’t you ever want to get rewards? Now you have a chance. Let us help you solve the obstinate Sudoku. And perhaps along the way we can learn Haskell, type design, and algorithm analysis.

One of the most famous solutions is Norvig’s sudoku solver which also implemented in different languages such as C++, Java, Ruby and Haskell, Norvig’s approach includes two crucial ideas: constraint propagation and search, In the Haskell version, the author offers a pretty straightforward implementation by using modules such as Data.list, Data.Map and Control.Monad, we can do some refactoring by taking this articles as reference.
Peter Norvig's constraint propagation algorithm is learned and adopted by many people to solve Sudoku problems. And Emmanuel Delaborde's Haskell version is truly beautiful. Maybe Grid's abstraction is very clever, foldM is used concisely. Our plan is generating an algorithm to solve other constraint satisfaction problems such as tic-tac-toe.


### How to run

Install Cable
https://cabal.readthedocs.io/en/3.4/index.html

cabal run :sudokuSolver

### important objects

Below are important objects that will be represented in our project:

Cell:: Int | Maybe(Int)

There are 2 cases for the content of a cell, it could either be a given digit, which is fixed, or a possible digit.


Row:: [Cell]

A row is a list of cells. For a 9*9 Sudoku puzzles, there would be 9 cells in a row.



Col:: [Cell]

This is basically the same idea as Row, the only difference is the cells are lined up in a vertical way in the puzzle.


Square:: [[Cell]]

A square is a 3*3 Digits list. For a 9*9 sudoku, there are 9 squares. 

The sudoku problem can be generalized into CSP wherein the values in the cell correspond to values of the variables. The domain is the scope of the variable which ranges across all digits between 1 and 9 for any variable initially (except for the variables that have already been assigned). The constraints for the sudoku problem is that the variables in all rows and columns are different from each other and the variables inside the square also need to be different.
