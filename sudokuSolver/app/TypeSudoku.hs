module Type where


-- Types
type Digit  = Char
type Square = String
type Unit   = [Square]

-- We represent our grid as a Map
type Grid = M.Map Square [Digit]

-- Setting Up the Problem

rows = "ABCDEFGHI"
cols = "123456789"
digits = "123456789"

cross :: String -> String -> [String]
cross rows cols = [ r:c:[] | r <- rows, c <- cols ]

squares :: [Square]
squares = cross rows cols  -- ["A1","A2","A3",...]
