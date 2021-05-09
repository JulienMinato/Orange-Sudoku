-- From https://www-proquest-com.ezproxy.proxy.library.oregonstate.edu/docview/926288615/E86931B8711A4F79PQ/1?accountid=13013

--Constraint Satisfaction Problems
--Binary CSPs
-- A binary constraint satisfaction problem is described by
-- • a set of variables V = {v1, v2,...,vm};
-- • for each variable vi, a finite set Di of possible values (its domain); and
-- • for each pair of distinct variables vi and vj, i<j, a binary relation Rij ⊆
--    Di × Dj, representing a constraint on the values that vi and vj can take on
--    simultaneously.

-- An assignment vi:=xi associates a variable vi to some value xi ∈ Di. A state is a
-- set of assignments, with at most one assignment per variable. A state S0 extends
-- state S if it contains all the assignments of S together with one or more additional
-- assignments.

-- A pair of assignments vi:=xi and vj:=xj, i<j, satisfies the corresponding constraint Rij if (xi, xj) ∈ Rij. 

-- A state is consistent if every pair of distinct assignments

-- in the state satisfies the corresponding constraint; otherwise it is inconsistent.

-- A state is complete if it assigns all the variables of V; otherwise it is partial. 

--  A solution to a CSP is any complete consistent state.


module CSP where


type Var = Int
type Value = Int

data Assignment = Var := Value

var :: Assignment -> Var
var (var := _) = var

value :: Assignment -> Value
value (_ := val) = val

type Relation = Assignment -> Assignment -> Bool

data CSP = CSP {vars, vals :: Int, rel :: Relation}

data State = State ([Assignment],[Var])

assignments :: State -> [Assignment]
assignments (State(as,_)) = as

unassigned :: State -> [Var]
unassigned (State(_,us)) = us

emptyState :: CSP -> State
emptyState CSP{vars=vars} = State([],[1..vars])

isEmptyState :: State -> Bool
isEmptyState = null . assignments

extensions :: CSP -> State -> [State]
extensions CSP{vals=vals} (State(as,nextvar:rest)) =
[State((nextvar := val):as,rest) | val <- [1..vals]]
extensions _ (State(_,[])) = []


newNextVar :: State -> Var -> State
newNextVar s@(State(as,[])) _ = s
newNextVar (State(as,us)) next = State(as,next:delete next us)


complete :: State -> Bool
complete = null . unassigned


lastAssignment :: State -> Assignment
lastAssignment = head . assignments


nextVar :: State -> Var
nextVar = head . unassigned


generate :: CSP -> [State]
generate csp@CSP{vars=vars} = g vars
whereg0= [emptyState csp]
g var = concat [extensions csp st | st <- g (var-1)]

inconsistencies :: CSP -> State -> [(Var, Var)]
inconsistencies CSP{rel=rel} st =
[ (var a, var b) | a <- as, b <- as, var a > var b, not (rel a b) ]
          where as = assignments st


consistent :: CSP -> State -> Bool
consistent csp = null . (inconsistencies csp)

test :: CSP -> [State] -> [State]
test csp = filter (consistent csp)

solver :: CSP -> [State]
solver csp = test csp candidates 
    where candidates = generate csp