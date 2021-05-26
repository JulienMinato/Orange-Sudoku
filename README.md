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

  Step 3: Run the command ‘ $ cabal init --interactive && cabal run ’

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
      
    - Others:
    
      - one possible solution on page 5
      
        https://www.cs.tufts.edu/~nr/cs257/archive/andrew-tolmach/modular_lazy_search_jfp.pdf
        
      - dcfl
        http://poincare.github.io/DCFL/
        
      - Eric
        https://github.com/walkie/CSP-Solver/blob/master/CSP.hs
        
      - NORDIN, T., & TOLMACH, A. (2001). Modular lazy search for constraint satisfaction problems. Journal of Functional Programming, 11(5), 557-587











