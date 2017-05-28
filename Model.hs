module Model where
import Utils

import Debug.Trace (trace)

-- | Hex consists of HexValue and coordinates(colummn, row)
data Hex = Hex HexValue Int Int deriving (Eq, Show)
data HexValue = Empty | A | B | C | D | E | F | G deriving (Eq, Show)

data State = State [[Hex]] Int deriving (Show)

-- | Extracts data from state
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
