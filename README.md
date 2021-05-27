# Solver :: CSP a -> Sudoku a -> :) 

Solving Every Sudoku Puzzle. 

As computer security expert Ben Laurie has stated, Sudoku is "a denial of service attack on human intellect". Many people we know were infected by the virus, and we thought maybe this would demonstrate that they didn't need to spend any more time on Sudoku. You may see a lot of Sudoku behind magazines or newspapers such as the Baro. Don’t you ever want to get rewards? Now you have a chance. Let us help you solve the obstinate Sudoku with our stable class libraries. And perhaps along the way we can learn Haskell, type design, and algorithm analysis.


Our project is to implement a CSP solver called puzzle killer, aiming to help artificial intelligence researchers solve CSPs through efficient and reliable algorithms and stable libraries. And for the people who are interested in puzzle games such as Sudoku, N-Queens, this tool can also provide them corresponding solutions quickly and painlessly.


The CSP contains n variables and x1,x2,...,xn that need to be evaluated and n domains d1,d2,...,dn such each domain could contain b possible values for each variable xi. CSP also has e constraints c1,c2,...,cewherein each constraint limits the values taken by one or more variables. The objective of this CSP problem is to come up with values of these variables such that all the constraints are satisfied. Generalized constraint satisfaction problem solver (CSP solver) can have multiple applications. In this case CSP will be used as the Sudoku Solver and provide a solution to the famous N-Queens problem.

***********************
### Milestone #1
***********************

- Project goal description and current progress:

  Our final goal of this project is the implementation for solving constraint satisfaction problems. People who are interested in puzzle games related to CSPs such as Sudoku, N-Queens, can also use this tool for obtaining quick results.
 
- Current progress and challenges:

  For now, we are trying to describe the CSP by defining variables, domain and constraint relation etc. The Var represents the variables and value is the domain for these variables. Both of them are defined. The emptyState is defined and the function isEmptyState checks whether the State is empty. The assignments keep track of the assigned state of variables. The constraints are defined in the CSP. The solver generates a set of solutions using generate and pruned using the test function. The data types and methods are defined. Presently the issue that is encountered is the display pattern for NQueens is not aligned appropriately. The implementation for Sudoku solver will be completed in Milestone 2. 
  
  We encountered the problem of imperfect printing of CSP results. For now we just print out the result as a list of states since the definition of Assignment using "or" which is convenient for us to deal with unfinished assignments. But this makes implementing the printing function a little challenging. To solve this  problem, We tried to use regular expressions and showing the interface to deal with these highly messy strings but it didn't work as we expected.


- How to run our project:

  Step 1: Install Cabal from https://www.haskell.org/cabal/ , run '$ cabal install --lib csp"

  Step 2: Open terminal and cd to the folder: /Orange-Sudoku/sudokuSolver

  Step 3: Run the command ‘ $ cabal run ’. 

  Step 4: Enter a number for the question: "N queens puzzle: how many queens?" and you will have all the answers!
  
  

- Design questions for workshop discussion:

  - For now our first version didn’t use monad (describe what’s included inside the code), will it be better or worse if we use monad?

  - Is it better to use higher order functions for defining constraints?

  - Is it advisable to use lazy data structure for storing intermediate values to reduce computation cost?

  - Is it better to use modular structure to separate generation and testing the potential candidate solutions? 
 
  - How are we going to present the results of N-Queens and Sudoku?


***********************
### Milestone #2
***********************

- Project description(updated):

   The function queens takes the input of how many queens need to be placed and it formulates it into a CSP and outputs CSP. The constraints used in this case are if there is a queen q1 is on col i then another queen q2 should not be placed on the same column, col i. Similarly the other constraint is if queen q1 is placed on row i then another queen q2 should not be placed on the same row, row i. The other constraint in this case is that if there is a queen q1 on col i and row j then another queen q2 should not be placed on col m and row n such that abs (col i - col m) = abs (row j - row n). The CSP solver is used to solve this CSP formulation. nQueenPrint is used for pretty printing the solution. getNum is a guard that is used to ensure that the argument for queens is numeric.

   The function graphcoloring takes the input of the number of the vertices in the graph, positions of the vertices in the graph and number of colors. The constraint in graph coloring is that no neighbor vertices have the same color or if vertices have same color they are not connected. The graph coloring function formulates and outputs the CSP. The mapM_ is used to print the result. In this case mapM_ is used as there is nothing to be returned. 
 
   The existing CSP was not able to solve sudoku as our constraints only took two arguments.Therefore Monad CSP library Control.Monad.CSP was used to solve Sudoku. solveSudoku takes list of lists of a. The oneCSPSolution outputs the first solution from the list of solutions to reduce the computation time. 
mapM_ assertRowConstraints dvs
mapM_ assertRowConstraints $ transpose dvs
sequence_ [assertSquareConstraints dvs x y | x <- [0,3,6], y <- [0,3,6]]
These three steps apply row, column and square constraints. 

- How to run our project:

  Step 1: Install Cabal from https://www.haskell.org/cabal/ run '$ cabal install --lib csp' 

  Step 2: Open terminal and cd to the folder: /Orange-Sudoku/sudokuSolver

  Step 3: Run the command ‘ $ cabal run ’, if succeed, you will see 3 types of CSPs. 

  Step 4: Type '1' for testing our N-Queen problem, '2' for Soduku, '3' for Graghcoloring. For the N-Queen, it only works when input is at least 4. The basic rule of Graphcoloring problem is two same color vertexes can not be connected to each other, our implementation just shows all the solutions on the terminal directly, the left hand side of ":=" represents the position while the right hand side is the color option. For now, we are using 5 vertexes 3 color, the output still has the type constructor "State" but it is readable. We will try to do this in a more aesthetic way (like the NQueen problem) if possible.
  Here is a picture for better understanding the graghcoloring problem:
 
  ![graphcolor](https://user-images.githubusercontent.com/56494388/119771665-e73cc200-be72-11eb-9973-c7d8c9294e6e.png)

  Step 5: The program will quit automatically after finishing one round, just using 'cabal run' to keep testing.
  
  
  P.S. If you still having trouble running our project, here is a walk-through video:
  
  https://www.youtube.com/watch?v=3TzLcpQFc4E

- Design decisions:

   Presently list comprehension and eta reductions were used instead of monadic notations for CSP, nQueens and graph coloring. However monadic notations were used for solving sudoku wherein mapM_ was used to map constraints. We plan to represent assignments as a Data.Map from Var to [Value]. We plan to use constraint that has a set of k variables, and a k-arity relationship between those variables that must hold true for the constraint to be satisfied.The variables will be initialized from the range of values in the domain and the values that violate the constraint will be pruned until the last value is left. The assignments will be stored in map which will be the state for the State Monad. We will also explore monad transformers as reader monad can be used to keep track of constraints. 

   Lazy modular approach is used that differentiates between generation and testing of likely solutions into different functions that connect with each other with lazy data structures. This makes it easier to read, write and modify. In this project lazy tree structure is used wherein the tree node denotes state and the leaf nodes denote the solutions. Lazy evaluation helps in generating list elements on demand and elements that do not pass the pruning stage are filtered out. In other words the program will not compute a candidate until it is ready to test it for consistency automatically. This algorithm also ensures that there is at most 1 inconsistent variable as it only needs to decide whether consistent is set to null. However computation cost is not reduced as many calculations are still performed per state and also check it for consistency.


















*************************
***External References***
*************************

- Constraint Satisfaction Problems 101

  https://en.wikipedia.org/wiki/Constraint_satisfaction_problem
  
  https://www.cpp.edu/~ftang/courses/CS420/notes/CSP.pdf
  
  https://www.slideshare.net/davidoverton/constraint-programming-in-haskell?from_action=save

- Solutions
    - Using Monad:
    
      https://hackage.haskell.org/package/csp
      
      http://overtond.blogspot.com/2008/07/pre.html
      
      https://github.com/abarbu/csp-haskell
      
      http://hackage.haskell.org/package/csp-1.4.0/docs/Control-Monad-CSP.html
      
    - Others:
    
      - one possible solution on page 5
      
        https://www.cs.tufts.edu/~nr/cs257/archive/andrew-tolmach/modular_lazy_search_jfp.pdf
        
      - dcfl
        http://poincare.github.io/DCFL/
        
      - Eric
        https://github.com/walkie/CSP-Solver/blob/master/CSP.hs
        
      - NORDIN, T., & TOLMACH, A. (2001). Modular lazy search for constraint satisfaction problems. Journal of Functional Programming, 11(5), 557-587











