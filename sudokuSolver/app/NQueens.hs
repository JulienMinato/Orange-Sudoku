-- | Example: N-Queens Problem
module NQueens where

import CSP


queens :: Int -> CSP
queens n = CSP{vals=n,vars=n,rel=safe}
  where safe (col1 := row1) (col2 := row2) = (row1 /= row2) && abs (col1 - col2) /= abs (row1 - row2)
