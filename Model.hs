module Model where
import Utils

import Debug.Trace (trace)

-- |Hex consists of HexValue and coordinates(colummn, row)
data Hex = Hex HexValue Int Int deriving (Eq, Show)
data HexValue = Empty | A | B | C | D | E | F | G deriving (Eq, Show)

data State = State [[Hex]] Int deriving (Show)

-- |Extracts data from state
value (Hex val _ _) = val
getRow (Hex _ row _) = row
getColumn (Hex _ _ column) = column
getHoneycomb (State honeycomb _) = honeycomb
getHex (State honeycomb _) row column = (honeycomb !! row) !! column
getN (State _ n) = n

isLastRow (Hex _ row _) (State _ n) = row + 1 == n
isTopRow (Hex _ row _) _ = row == 0
isLastColumn (Hex _ row column) (State _ n) | even row  = column + 1 == n - 1
                                            | otherwise = column + 1 == n
isFirstColumn (Hex _ _ column) _ = column == 0

-- |Creates new state by inserting hex into an old state
insertHex hex (State honeycomb n) = State (insertHexInHoneycomb hex honeycomb 0) n
insertHexInHoneycomb hex (row:rows) currRowIndex | (getRow hex) == currRowIndex = (removeThanInsertAt hex row (getColumn hex)):rows
                                                 | otherwise                    = row:(insertHexInHoneycomb hex rows (currRowIndex + 1)) 
--insertHexInRow hex (h:hs) currentHIndex | (getColumn hex) == currentHIndex = hex:hs
--                                        | otherwise                        = h:(insertHexInRow hex hs (currentHIndex+1))
--TESTS
-- insertHex (Hex A 2 1) (State [[Hex A 0 0, Hex B 0 1], [Hex C 1 0, Hex D 1 1, Hex E 1 2], [Hex F 2 0, Hex G 2 1]] 3) == State [[Hex A 0 0,Hex B 0 1],[Hex C 1 0,Hex D 1 1,Hex E 1 2],[Hex F 2 0,Hex A 2 1]] 3


