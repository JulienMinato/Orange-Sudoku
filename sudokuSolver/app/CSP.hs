module CSP where

import Data.List


--The Definition comes from
-- NORDIN, T., & TOLMACH, A. (2001). Modular lazy search for constraint satisfaction problems. 
-- Journal of Functional Programming, 11(5), 557-587. 

--Constraint Satisfaction Problems
--Binary CSPs
-- A binary constraint satisfaction problem is described by
-- • a set of variables V = {v1, v2,...,vm};
-- • for each variable vi, a finite set Di of possible values (its domain); and
-- • for each pair of distinct variables vi and vj, i<j, a binary relation Rij ⊆
--    Di × Dj, representing a constraint on the values that vi and vj can take on
--    simultaneously.




-- A set of variables V
type Var = Int

--its domain: for each variable vi, a finite set of possible values 
type Value = Int

-- An assignment vi := xi associates a variable vi to some value xi ∈ Di.
--(:=) is actually a constructor. It builds up a datatype. 
-- We can't know what data type it constructs without knowing
--  Read it as "translates to" or "is isomorphic to".

data Assignment = Var := Value
  deriving (Eq,Show)
--True for taking two assignments if the assignments obey the relevant constraint. 
type Relation = Assignment -> Assignment -> Bool

-- A state is a set of assignments and consistent if every pair of distinct assignments
data State = State ([Assignment],[Var])
  deriving (Eq,Show)

--A CSP is modeled record containing the number of variables, vars, 
--the size of domain, vals, 
--and a constraint relation, rel
data CSP = CSP {vars, vals :: Int, rel :: Relation}
--   deriving (Eq,Show)

-- instance Show CSP where
--   show (CSP...)=...


-- putStrLn ...
-- instance Show CSP where
--    show (CSP ...) = ...
--  All domains have the same size n and that their values are represented by integers in the set {1, 2, . . . , n}; 
--  these limitations could be trivially removed.

var :: Assignment -> Var
var (var := _) = var

value :: Assignment -> Value
value (_ := val) = val


-- A pair of assignments vi:=xi and vj:=xj, i<j, satisfies the corresponding constraint Rij if (xi, xj) ∈ Rij. 
-- A state is modeled as a sequence of assignments, together with a sequence of as yet unassigned variables.
assignments :: State -> [Assignment]
assignments (State(as,_)) = as

unassigned :: State -> [Var]
unassigned (State(_,us)) = us

-- States are built from emptyState by repeated use of extensions, 
-- which takes a state, extracts the head (if any) of its list of unassigned variables, 
-- constructs assignments of this variable to each possible value, extends the original state 
-- with each of these assignments in turn, and returns the resulting list of extended states.
emptyState :: CSP -> State
emptyState CSP{vars=vars} = State([],[1..vars])

isEmptyState :: State -> Bool
isEmptyState = null . assignments


-- A state S' extends state S if it contains all the assignments of S together with one or more additional assignments.
extensions :: CSP -> State -> [State]
extensions CSP{vals=vals} (State(as,nextvar:rest)) = [State((nextvar := val):as,rest) | val <- [1..vals]]
extensions _ (State(_,[])) = []


-- A state is complete if it assigns all the variables; otherwise it is partial. 
complete :: State -> Bool
complete = null . unassigned


--The lastAssignment operator returns the assignment with which the state was most recently extended.
lastAssignment :: State -> Assignment
lastAssignment = head . assignments


nextVar :: State -> Var
nextVar = head . unassigned



--Binary CSPs can be generalized by replacing the binary relations by n-ary relations. 
generate :: CSP -> [State]
generate csp@CSP{vars=vars} = g vars
  where g 0 = [emptyState csp]
        g var = concat [extensions csp st | st <- g (var-1)]




-- A state is consistent if every pair of distinct assignments in the state satisfies the corresponding constraint; 
-- otherwise it is inconsistent.
-- Consistent actually demands just enough of the list to check whether it is null,
-- and hence at most one inconsistent variable pair is calculated
consistent :: CSP -> State -> Bool
consistent csp = null . (inconsistencies csp)

--Inconsistencies appears to build a list of all inconsistent variable pairs in the state
inconsistencies :: CSP -> State -> [(Var, Var)]
inconsistencies CSP{rel=rel} st = [(var a, var b) | a <- as, b <- as, var a > var b, not (rel a b)]
          where as = assignments st



test :: CSP -> [State] -> [State]
test csp = filter (consistent csp)


----A solution of a CSP is a total assignation which satisfied all the constraints of the problem.
--A solution to a CSP is any complete consistent state
--the solver returns a list of all solutions if demanded, 
--it can be used to obtain just the first solution
--But solver is still extremely inefficient because it duplicates work
solver :: CSP -> [State]
solver csp = test csp candidates 
    where candidates = generate csp



-- The graphcoloring function constructs an instance of a graph coloring, 
-- specified by a number of nodes, a set of edges between nodes
--  (represented by a characteristic function on pairs of nodes),
--   and a number of colors. The CSP variables are the graph nodes, 
--   the values are the possible colors, and the oracle function returns True on 
--   a pair of color assignments provided that the colors are different or there is no edge between the nodes.


-- graphcoloring :: Int -> ((Var,Var) -> Bool) -> Int -> CSP
-- graphcoloring nodes adj colors = CSP{vars=nodes,vals=colors,rel=ok}
--         where ok (n1 := c1) (n2 := c2) = c1 /= c2 || not (adj (n1,n2))


